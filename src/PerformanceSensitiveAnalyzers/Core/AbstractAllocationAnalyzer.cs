﻿// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Immutable;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Microsoft.CodeAnalysis.PerformanceSensitiveAnalyzers
{
    internal abstract class AbstractAllocationAnalyzer : DiagnosticAnalyzer
    {
        protected abstract ImmutableArray<OperationKind> Operations { get; }

        protected abstract void AnalyzeNode(OperationAnalysisContext context);

        public override void Initialize(AnalysisContext context)
        {
            if (Operations.IsEmpty)
            {
                return;
            }

            context.RegisterCompilationStartAction(compilationStartContext =>
            {
                compilationStartContext.RegisterOperationBlockStartAction(blockStartContext =>
                {
                    RegisterOperationAnalysis(blockStartContext);
                });
            });
        }

        private void RegisterOperationAnalysis(OperationBlockStartAnalysisContext operationBlockStartAnalysisContext)
        {
            operationBlockStartAnalysisContext.RegisterOperationAction(
                syntaxNodeContext =>
                {
                    AnalyzeNode(syntaxNodeContext);
                },
                Operations);
        }
    }
}
