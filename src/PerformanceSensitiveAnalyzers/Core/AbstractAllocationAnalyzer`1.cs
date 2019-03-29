// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Immutable;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Microsoft.CodeAnalysis.PerformanceSensitiveAnalyzers
{
    internal abstract class AbstractAllocationAnalyzer<TLanguageKindEnum>
            : AbstractAllocationAnalyzer
            where TLanguageKindEnum : struct
    {
        protected abstract ImmutableArray<TLanguageKindEnum> Expressions { get; }

        protected sealed override ImmutableArray<OperationKind> Operations => ImmutableArray<OperationKind>.Empty;

        protected abstract void AnalyzeNode(SyntaxNodeAnalysisContext context);

        protected override void AnalyzeNode(OperationAnalysisContext context) { }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterCompilationStartAction(compilationStartContext =>
            {
                compilationStartContext.RegisterCodeBlockStartAction<TLanguageKindEnum>(blockStartContext =>
                {
                    RegisterSyntaxAnalysis(blockStartContext);
                });
            });
        }

        private void RegisterSyntaxAnalysis(CodeBlockStartAnalysisContext<TLanguageKindEnum> codeBlockStartAnalysisContext)
        {
            codeBlockStartAnalysisContext.RegisterSyntaxNodeAction(
                syntaxNodeContext =>
                {
                    AnalyzeNode(syntaxNodeContext);
                },
                Expressions);
        }
    }
}
