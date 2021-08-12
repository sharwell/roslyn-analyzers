// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using Analyzer.Utilities;
using Analyzer.Utilities.Extensions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;

namespace Microsoft.NetCore.Analyzers.ImmutableCollections
{
    /// <summary>
    /// CA2009: Do not call ToImmutableCollection on an ImmutableCollection value
    /// </summary>
    [DiagnosticAnalyzer(LanguageNames.CSharp, LanguageNames.VisualBasic)]
    public sealed class DoNotCallToImmutableCollectionOnAnImmutableCollectionValueAnalyzer : DiagnosticAnalyzer
    {
        private const string ImmutableArrayMetadataName = "System.Collections.Immutable.ImmutableArray`1";
        internal const string RuleId = "CA2009";

        private static readonly LocalizableString s_localizableTitle = new LocalizableResourceString(nameof(MicrosoftNetCoreAnalyzersResources.DoNotCallToImmutableCollectionOnAnImmutableCollectionValueTitle), MicrosoftNetCoreAnalyzersResources.ResourceManager, typeof(MicrosoftNetCoreAnalyzersResources));
        private static readonly LocalizableString s_localizableMessage = new LocalizableResourceString(nameof(MicrosoftNetCoreAnalyzersResources.DoNotCallToImmutableCollectionOnAnImmutableCollectionValueMessage), MicrosoftNetCoreAnalyzersResources.ResourceManager, typeof(MicrosoftNetCoreAnalyzersResources));

        internal static DiagnosticDescriptor Rule = DiagnosticDescriptorHelper.Create(RuleId,
                                                                             s_localizableTitle,
                                                                             s_localizableMessage,
                                                                             DiagnosticCategory.Reliability,
                                                                             RuleLevel.IdeSuggestion,
                                                                             description: null,
                                                                             isPortedFxCopRule: false,
                                                                             isDataflowRule: false);

        private static readonly ImmutableDictionary<string, string> ImmutableCollectionMetadataNames = new Dictionary<string, string>
        {
            ["ToImmutableArray"] = "System.Collections.Immutable.ImmutableArray`1",
            ["ToImmutableDictionary"] = "System.Collections.Immutable.ImmutableDictionary`2",
            ["ToImmutableHashSet"] = "System.Collections.Immutable.ImmutableHashSet`1",
            ["ToImmutableList"] = "System.Collections.Immutable.ImmutableList`1",
            ["ToImmutableSortedDictionary"] = "System.Collections.Immutable.ImmutableSortedDictionary`2",
            ["ToImmutableSortedSet"] = "System.Collections.Immutable.ImmutableSortedSet`1",
        }.ToImmutableDictionary();

        public static ImmutableArray<string> ToImmutableMethodNames => ImmutableCollectionMetadataNames.Keys.ToImmutableArray();

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        private class TestExample2 : IncrementalDiagnosticAnalyzer
        {
            public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

            public override void Initialize(IncrementalAnalysisContext context)
            {
                IncrementalValueProvider<WellKnownTypeProvider> typeProviderProvider = context.CompilationProvider.Select(
                    static (compilation, cancellationToken) => WellKnownTypeProvider.GetOrCreate(compilation));

                IncrementalValueProvider<INamedTypeSymbol?> immutableArraySymbolProvider = typeProviderProvider.Select(
                    static (typeProvider, cancellationToken) => typeProvider.GetOrCreateTypeByMetadataName(ImmutableArrayMetadataName));

                IncrementalValueProvider<IAssemblySymbol?> immutableCollectionsAssemblyProvider = immutableArraySymbolProvider.Select(
                    static (immutableArraySymbol, cancellationToken) => immutableArraySymbol?.ContainingAssembly);

                IncrementalValuesProvider<IInvocationOperation> invocationOperationProvider = context.OperationProvider.CreateOperationProvider(
                    static (operation, cancellationToken) => operation.Kind == OperationKind.Invocation,
                    static (context, cancellationToken) => (IInvocationOperation)context.Operation);

                IncrementalValueProvider<(Compilation compilation, IAssemblySymbol? immutableCollectionsAssembly)> commonInputProvider = context.CompilationProvider
                    .Combine(immutableCollectionsAssemblyProvider)
                    .Select(static (context, cancellationToken) => (compilation: context.Left, immutableCollectionsAssembly: context.Right));

                IncrementalValuesProvider<Diagnostic> diagnosticProvider = invocationOperationProvider.Combine(commonInputProvider).SelectMany(
                    static (context, cancellationToken) =>
                    {
                        var invocation = context.Left;
                        var compilation = context.Right.compilation;
                        var immutableCollectionsAssembly = context.Right.immutableCollectionsAssembly;
                        if (immutableCollectionsAssembly is null)
                        {
                            return ImmutableArray<Diagnostic>.Empty;
                        }

                        var targetMethod = invocation.TargetMethod;
                        if (targetMethod == null || !ImmutableCollectionMetadataNames.TryGetValue(targetMethod.Name, out string metadataName))
                        {
                            return ImmutableArray<Diagnostic>.Empty;
                        }

                        Debug.Assert(!string.IsNullOrEmpty(metadataName));

                        // Do not flag invocations that take any explicit argument (comparer, converter, etc.)
                        // as they can potentially modify the contents of the resulting collection.
                        var argumentsToSkip = invocation.IsExtensionMethodAndHasNoInstance() ? 1 : 0;
                        if (invocation.Arguments.Skip(argumentsToSkip).Any(arg => arg.ArgumentKind == ArgumentKind.Explicit))
                        {
                            return ImmutableArray<Diagnostic>.Empty;
                        }

                        var immutableCollectionType = immutableCollectionsAssembly.GetTypeByMetadataName(metadataName);
                        if (immutableCollectionType == null)
                        {
                            // The user might be running against a custom system assembly that defines ImmutableArray,
                            // but not other immutable collection types.
                            return ImmutableArray<Diagnostic>.Empty;
                        }

                        var receiverType = invocation.GetReceiverType(compilation, beforeConversion: true, cancellationToken);
                        if (receiverType != null &&
                            receiverType.DerivesFromOrImplementsAnyConstructionOf(immutableCollectionType))
                        {
                            return ImmutableArray.Create(invocation.CreateDiagnostic(
                                Rule,
                                targetMethod.Name,
                                immutableCollectionType.Name));
                        }

                        return ImmutableArray<Diagnostic>.Empty;
                    });

                context.RegisterDiagnosticOutput(
                    diagnosticProvider,
                    (context, diagnostic) => context.ReportDiagnostic(diagnostic));
            }
        }

        public override void Initialize(AnalysisContext context)
        {
            context.EnableConcurrentExecution();
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);

            context.RegisterCompilationStartAction(compilationStartContext =>
            {
                var compilation = compilationStartContext.Compilation;
                var wellKnownTypeProvider = WellKnownTypeProvider.GetOrCreate(compilation);
                var immutableArraySymbol = wellKnownTypeProvider.GetOrCreateTypeByMetadataName(ImmutableArrayMetadataName);
                if (immutableArraySymbol is null)
                {
                    return;
                }

                var immutableCollectionsAssembly = immutableArraySymbol.ContainingAssembly;

                compilationStartContext.RegisterOperationAction(operationContext =>
                {
                    var invocation = (IInvocationOperation)operationContext.Operation;
                    var targetMethod = invocation.TargetMethod;
                    if (targetMethod == null || !ImmutableCollectionMetadataNames.TryGetValue(targetMethod.Name, out string metadataName))
                    {
                        return;
                    }

                    Debug.Assert(!string.IsNullOrEmpty(metadataName));

                    // Do not flag invocations that take any explicit argument (comparer, converter, etc.)
                    // as they can potentially modify the contents of the resulting collection.
                    var argumentsToSkip = invocation.IsExtensionMethodAndHasNoInstance() ? 1 : 0;
                    if (invocation.Arguments.Skip(argumentsToSkip).Any(arg => arg.ArgumentKind == ArgumentKind.Explicit))
                    {
                        return;
                    }

                    var immutableCollectionType = immutableCollectionsAssembly.GetTypeByMetadataName(metadataName);
                    if (immutableCollectionType == null)
                    {
                        // The user might be running against a custom system assembly that defines ImmutableArray,
                        // but not other immutable collection types.
                        return;
                    }

                    var receiverType = invocation.GetReceiverType(operationContext.Compilation, beforeConversion: true, cancellationToken: operationContext.CancellationToken);
                    if (receiverType != null &&
                        receiverType.DerivesFromOrImplementsAnyConstructionOf(immutableCollectionType))
                    {
                        operationContext.ReportDiagnostic(
                            invocation.CreateDiagnostic(
                                Rule,
                                targetMethod.Name,
                                immutableCollectionType.Name));
                    }
                }, OperationKind.Invocation);
            });
        }
    }

    internal static class IncrementalValueProviderExtensions
    {
        public static IncrementalValueProvider<TResult> Select<TSource, TResult>(this IncrementalValueProvider<TSource> source, Func<TSource, CancellationToken, TResult> selector)
        {
            throw new NotImplementedException();
        }

        public static IncrementalValuesProvider<TResult> Select<TSource, TResult>(this IncrementalValuesProvider<TSource> source, Func<TSource, CancellationToken, TResult> selector)
        {
            throw new NotImplementedException();
        }

        public static IncrementalValuesProvider<TResult> SelectMany<TSource, TResult>(this IncrementalValueProvider<TSource> source, Func<TSource, CancellationToken, ImmutableArray<TResult>> selector)
        {
            throw new NotImplementedException();
        }

        public static IncrementalValuesProvider<TResult> SelectMany<TSource, TResult>(this IncrementalValueProvider<TSource> source, Func<TSource, CancellationToken, IEnumerable<TResult>> selector)
        {
            throw new NotImplementedException();
        }

        public static IncrementalValuesProvider<TResult> SelectMany<TSource, TResult>(this IncrementalValuesProvider<TSource> source, Func<TSource, CancellationToken, ImmutableArray<TResult>> selector)
        {
            throw new NotImplementedException();
        }

        public static IncrementalValuesProvider<TResult> SelectMany<TSource, TResult>(this IncrementalValuesProvider<TSource> source, Func<TSource, CancellationToken, IEnumerable<TResult>> selector)
        {
            throw new NotImplementedException();
        }

        public static IncrementalValueProvider<ImmutableArray<TSource>> Collect<TSource>(this IncrementalValuesProvider<TSource> source)
        {
            throw new NotImplementedException();
        }

        public static IncrementalValuesProvider<(TLeft Left, TRight Right)> Combine<TLeft, TRight>(this IncrementalValuesProvider<TLeft> provider1, IncrementalValueProvider<TRight> provider2)
        {
            throw new NotImplementedException();
        }

        public static IncrementalValueProvider<(TLeft Left, TRight Right)> Combine<TLeft, TRight>(this IncrementalValueProvider<TLeft> provider1, IncrementalValueProvider<TRight> provider2)
        {
            throw new NotImplementedException();
        }

        public static IncrementalValuesProvider<TSource> Where<TSource>(this IncrementalValuesProvider<TSource> source, Func<TSource, bool> predicate)
        {
            throw new NotImplementedException();
        }

        public static IncrementalValueProvider<TSource> WithComparer<TSource>(this IncrementalValueProvider<TSource> source, IEqualityComparer<TSource> comparer)
        {
            throw new NotImplementedException();
        }

        public static IncrementalValuesProvider<TSource> WithComparer<TSource>(this IncrementalValuesProvider<TSource> source, IEqualityComparer<TSource> comparer)
        {
            throw new NotImplementedException();
        }
    }

    internal abstract class IncrementalDiagnosticAnalyzer : DiagnosticAnalyzer
    {
        public abstract void Initialize(IncrementalAnalysisContext context);

        public sealed override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze | GeneratedCodeAnalysisFlags.ReportDiagnostics);
            context.EnableConcurrentExecution();

            throw new NotSupportedException();
        }
    }

    internal readonly struct IncrementalAnalysisContext
    {
        public SyntaxValueProvider SyntaxProvider { get; }

        public OperationValueProvider OperationProvider { get; }

        public IncrementalValueProvider<Compilation> CompilationProvider { get; }

        public IncrementalValuesProvider<MetadataReference> MetadataReferencesProvider { get; }

        public IncrementalValueProvider<ParseOptions> ParseOptionsProvider { get; }

        public IncrementalValuesProvider<AdditionalText> AdditionalTextsProvider { get; }

        public IncrementalValueProvider<AnalyzerConfigOptionsProvider> AnalyzerConfigOptionsProvider { get; }

        public void RegisterDiagnosticOutput<TSource>(IncrementalValueProvider<TSource> source, Action<DiagnosticProductionContext, TSource> action)
            => throw new NotImplementedException();

        public void RegisterDiagnosticOutput<TSource>(IncrementalValuesProvider<TSource> source, Action<DiagnosticProductionContext, TSource> action)
            => throw new NotImplementedException();
    }

    public readonly struct DiagnosticProductionContext
    {
        public CancellationToken CancellationToken { get; }

        public void ReportDiagnostic(Diagnostic diagnostic)
            => throw new NotImplementedException();
    }

    internal readonly struct SyntaxValueProvider
    {
        public IncrementalValuesProvider<T> CreateSyntaxProvider<T>(Func<SyntaxNode, CancellationToken, bool> predicate, Func<AnalyzerSyntaxContext, CancellationToken, T> transform)
        {
            throw new NotImplementedException();
        }
    }

    internal readonly struct OperationValueProvider
    {
        public IncrementalValuesProvider<T> CreateOperationProvider<T>(Func<IOperation, CancellationToken, bool> predicate, Func<AnalyzerOperationContext, CancellationToken, T> transform)
        {
            throw new NotImplementedException();
        }
    }

    internal readonly struct AnalyzerSyntaxContext
    {
        public SyntaxNode Node { get; }
        public SemanticModel SemanticModel { get; }
    }

    internal readonly struct AnalyzerOperationContext
    {
        public IOperation Operation { get; }
    }

    internal readonly struct IncrementalValueProvider<TValue>
    {
    }

    internal readonly struct IncrementalValuesProvider<TValues>
    {
    }
}
