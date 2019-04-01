// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Linq;
using Analyzer.Utilities;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;

namespace Roslyn.Diagnostics.Analyzers
{
    [DiagnosticAnalyzer(LanguageNames.CSharp, LanguageNames.VisualBasic)]
    public sealed class InstanceIsNotCopyable : DiagnosticAnalyzer
    {
        private static readonly LocalizableString s_localizableTitle = new LocalizableResourceString(nameof(RoslynDiagnosticsAnalyzersResources.InstanceIsNotCopyableTitle), RoslynDiagnosticsAnalyzersResources.ResourceManager, typeof(RoslynDiagnosticsAnalyzersResources));
        private static readonly LocalizableString s_localizableMessage = new LocalizableResourceString(nameof(RoslynDiagnosticsAnalyzersResources.InstanceIsNotCopyableMessage), RoslynDiagnosticsAnalyzersResources.ResourceManager, typeof(RoslynDiagnosticsAnalyzersResources));
        private static readonly LocalizableString s_localizableDescription = new LocalizableResourceString(nameof(RoslynDiagnosticsAnalyzersResources.InstanceIsNotCopyableDescription), RoslynDiagnosticsAnalyzersResources.ResourceManager, typeof(RoslynDiagnosticsAnalyzersResources));

        internal static DiagnosticDescriptor InstanceIsNotCopyableRule = new DiagnosticDescriptor(
            RoslynDiagnosticIds.InstanceIsNotCopyableRuleId,
            s_localizableTitle,
            s_localizableMessage,
            DiagnosticCategory.RoslynDiagnosticsUsage,
            DiagnosticHelpers.DefaultDiagnosticSeverity,
            isEnabledByDefault: DiagnosticHelpers.EnabledByDefaultIfNotBuildingVSIX,
            description: s_localizableDescription,
            helpLinkUri: null,
            customTags: WellKnownDiagnosticTags.Telemetry);

        private static readonly LocalizableString s_localizableArgumentNotByValueTitle = new LocalizableResourceString(nameof(RoslynDiagnosticsAnalyzersResources.ArgumentNotByValueTitle), RoslynDiagnosticsAnalyzersResources.ResourceManager, typeof(RoslynDiagnosticsAnalyzersResources));
        private static readonly LocalizableString s_localizableArgumentNotByValueMessage = new LocalizableResourceString(nameof(RoslynDiagnosticsAnalyzersResources.ArgumentNotByValueMessage), RoslynDiagnosticsAnalyzersResources.ResourceManager, typeof(RoslynDiagnosticsAnalyzersResources));
        private static readonly LocalizableString s_localizableArgumentNotByValueDescription = new LocalizableResourceString(nameof(RoslynDiagnosticsAnalyzersResources.ArgumentNotByValueDescription), RoslynDiagnosticsAnalyzersResources.ResourceManager, typeof(RoslynDiagnosticsAnalyzersResources));

        internal static DiagnosticDescriptor ArgumentNotByValueRule = new DiagnosticDescriptor(
            RoslynDiagnosticIds.ArgumentNotByValueRuleId,
            s_localizableArgumentNotByValueTitle,
            s_localizableArgumentNotByValueMessage,
            DiagnosticCategory.RoslynDiagnosticsUsage,
            DiagnosticHelpers.DefaultDiagnosticSeverity,
            isEnabledByDefault: DiagnosticHelpers.EnabledByDefaultIfNotBuildingVSIX,
            description: s_localizableArgumentNotByValueDescription,
            helpLinkUri: null,
            customTags: WellKnownDiagnosticTags.Telemetry);

        private static readonly LocalizableString s_localizableNonCopyableContainingTypeTitle = new LocalizableResourceString(nameof(RoslynDiagnosticsAnalyzersResources.NonCopyableContainingTypeTitle), RoslynDiagnosticsAnalyzersResources.ResourceManager, typeof(RoslynDiagnosticsAnalyzersResources));
        private static readonly LocalizableString s_localizableNonCopyableContainingTypeMessage = new LocalizableResourceString(nameof(RoslynDiagnosticsAnalyzersResources.NonCopyableContainingTypeMessage), RoslynDiagnosticsAnalyzersResources.ResourceManager, typeof(RoslynDiagnosticsAnalyzersResources));
        private static readonly LocalizableString s_localizableNonCopyableContainingTypeDescription = new LocalizableResourceString(nameof(RoslynDiagnosticsAnalyzersResources.NonCopyableContainingTypeDescription), RoslynDiagnosticsAnalyzersResources.ResourceManager, typeof(RoslynDiagnosticsAnalyzersResources));

        internal static DiagnosticDescriptor NonCopyableContainingTypeRule = new DiagnosticDescriptor(
            RoslynDiagnosticIds.NonCopyableContainingTypeRuleId,
            s_localizableNonCopyableContainingTypeTitle,
            s_localizableNonCopyableContainingTypeMessage,
            DiagnosticCategory.RoslynDiagnosticsUsage,
            DiagnosticHelpers.DefaultDiagnosticSeverity,
            isEnabledByDefault: DiagnosticHelpers.EnabledByDefaultIfNotBuildingVSIX,
            description: s_localizableNonCopyableContainingTypeDescription,
            helpLinkUri: null,
            customTags: WellKnownDiagnosticTags.Telemetry);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(InstanceIsNotCopyableRule, ArgumentNotByValueRule, NonCopyableContainingTypeRule);

        public override void Initialize(AnalysisContext context)
        {
            context.EnableConcurrentExecution();
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze | GeneratedCodeAnalysisFlags.ReportDiagnostics);

            context.RegisterCompilationStartAction(context =>
            {
                var analyzer = new Analyzer();

                context.RegisterSymbolAction(analyzer.HandleField, SymbolKind.Field);

                context.RegisterOperationAction(analyzer.HandleConversion, OperationKind.Conversion);
                context.RegisterOperationAction(analyzer.HandleReturn, OperationKind.Return, OperationKind.YieldReturn);
                context.RegisterOperationAction(analyzer.HandleInvocation, OperationKind.Invocation);
                context.RegisterOperationAction(analyzer.HandleInterpolation, OperationKind.Interpolation);
            });
        }

        private class Analyzer
        {
            private readonly ConcurrentDictionary<ITypeSymbol, bool> _nonCopyableTypes = new ConcurrentDictionary<ITypeSymbol, bool>();

            internal void HandleInterpolation(OperationAnalysisContext context)
            {
                var interpolation = (IInterpolationOperation)context.Operation;
                var diagnostic = HandleConversion(interpolation.Expression.Syntax, context.Compilation.GetSpecialType(SpecialType.System_Object), interpolation.Expression);
                if (diagnostic != null)
                {
                    context.ReportDiagnostic(diagnostic);
                }
            }

            internal void HandleField(SymbolAnalysisContext context)
            {
                var field = (IFieldSymbol)context.Symbol;
                if (!IsNonCopyable(field.Type))
                {
                    return;
                }

                if (field.ContainingType.IsReferenceType)
                {
                    return;
                }

                if (!IsNonCopyable(field.ContainingType))
                {
                    // Types containing non-copyable fields must be non-copyable
                    context.ReportDiagnostic(Diagnostic.Create(NonCopyableContainingTypeRule, field.Locations.FirstOrDefault() ?? field.ContainingType.Locations.First()));
                }
            }

            internal void HandleConversion(OperationAnalysisContext context)
            {
                var operation = (IConversionOperation)context.Operation;
                var diagnostic = HandleConversion(operation.Syntax, operation.Type, operation.Operand);
                if (diagnostic != null)
                {
                    context.ReportDiagnostic(diagnostic);
                }
            }

            private Diagnostic? HandleConversion(SyntaxNode syntax, ITypeSymbol type, IOperation operand)
            {
                if (type is null)
                {
                    return null;
                }

                if (IsNonCopyable(type) && !IsMoved(operand))
                {
                    // Converting *to* a non-copyable type is an unboxing operation that makes a copy
                    return Diagnostic.Create(InstanceIsNotCopyableRule, syntax.GetLocation());
                }

                if (operand.Type != null && IsNonCopyable(operand.Type) && !IsMoved(operand))
                {
                    // Converting *from* a non-copyable type is a boxing operation that makes a copy
                    return Diagnostic.Create(InstanceIsNotCopyableRule, syntax.GetLocation());
                }

                return null;
            }

            internal void HandleReturn(OperationAnalysisContext context)
            {
                var operation = (IReturnOperation)context.Operation;
                if (operation.ReturnedValue?.Type is null)
                {
                    return;
                }

                if (IsNonCopyable(operation.ReturnedValue.Type) && !IsMoved(operation.ReturnedValue))
                {
                    // Return values must be moved
                    context.ReportDiagnostic(Diagnostic.Create(InstanceIsNotCopyableRule, operation.ReturnedValue.Syntax.GetLocation()));
                }
            }

            internal void HandleInvocation(OperationAnalysisContext context)
            {
                var operation = (IInvocationOperation)context.Operation;

                if (operation.Instance != null)
                {
                    Diagnostic? diagnostic = null;
                    if (operation.TargetMethod.ContainingType.Equals(operation.Instance.Type))
                    {
                        switch (operation.Instance.Kind)
                        {
                            case OperationKind.ParameterReference:
                                if (((IParameterReferenceOperation)operation.Instance).Parameter.RefKind == RefKind.In)
                                {
                                    diagnostic = Diagnostic.Create(InstanceIsNotCopyableRule, operation.Instance.Syntax.GetLocation());
                                }

                                break;

                            case OperationKind.FieldReference:
                                if (((IFieldReferenceOperation)operation.Instance).Field.IsReadOnly)
                                {
                                    diagnostic = Diagnostic.Create(InstanceIsNotCopyableRule, operation.Instance.Syntax.GetLocation());
                                }

                                break;

                            case OperationKind.LocalReference:
                                if (((ILocalReferenceOperation)operation.Instance).Local.RefKind == RefKind.RefReadOnly)
                                {
                                    diagnostic = Diagnostic.Create(InstanceIsNotCopyableRule, operation.Instance.Syntax.GetLocation());
                                }

                                break;

                            case OperationKind.ArrayElementReference:
                                break;

                            case OperationKind.InstanceReference:
                                break;

                            default:
                                throw new NotImplementedException();
                        }
                    }
                    else
                    {
                        diagnostic = HandleConversion(operation.Instance.Syntax, operation.TargetMethod.ContainingType, operation.Instance);
                    }

                    if (diagnostic != null)
                    {
                        context.ReportDiagnostic(diagnostic);
                    }
                }

                foreach (var argument in operation.Arguments)
                {
                    if (argument.Parameter.RefKind != RefKind.None)
                    {
                        // The parameter is passed by reference
                        continue;
                    }

                    if (argument.Value is object && IsNonCopyable(argument.Value.Type) && !IsMoved(argument.Value))
                    {
                        // Non-copyable arguments must be moved
                        context.ReportDiagnostic(Diagnostic.Create(ArgumentNotByValueRule, argument.Value.Syntax.GetLocation(), argument.Value.Type));
                    }
                }
            }

            private static bool IsMoved(IOperation operation)
            {
                switch (operation.Kind)
                {
                    case OperationKind.DefaultValue:
                    case OperationKind.ObjectCreation:
                    case OperationKind.Throw:
                        return true;

                    case OperationKind.Conversion:
                        return ((IConversionOperation)operation).Operand.Kind == OperationKind.Throw;

                    case OperationKind.Invocation:
                        var invocation = (IInvocationOperation)operation;
                        return Equals(invocation.TargetMethod.ContainingType, operation.Type)
                            && !invocation.TargetMethod.ReturnsByRef
                            && !invocation.TargetMethod.ReturnsByRefReadonly;

                    default:
                        return false;
                }
            }

            private bool IsNonCopyable(ITypeSymbol type)
            {
                if (type.Kind != SymbolKind.NamedType)
                {
                    return false;
                }

                if (_nonCopyableTypes.TryGetValue(type, out var nonCopyable))
                {
                    return nonCopyable;
                }

                return _nonCopyableTypes.GetOrAdd(type, key => key.OriginalDefinition.GetAttributes().Any(attribute =>
                {
                    if (attribute.AttributeClass.Name == "NonCopyableAttribute")
                        return true;

                    return false;
                }));
            }
        }
    }
}
