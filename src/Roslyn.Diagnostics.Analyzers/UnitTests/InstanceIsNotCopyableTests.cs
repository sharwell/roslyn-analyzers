// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Threading.Tasks;
using Xunit;
using VerifyCS = Test.Utilities.CSharpSecurityCodeFixVerifier<
    Roslyn.Diagnostics.Analyzers.InstanceIsNotCopyable,
    Microsoft.CodeAnalysis.Testing.EmptyCodeFixProvider>;
using VerifyVB = Test.Utilities.VisualBasicSecurityCodeFixVerifier<
    Roslyn.Diagnostics.Analyzers.InstanceIsNotCopyable,
    Microsoft.CodeAnalysis.Testing.EmptyCodeFixProvider>;

namespace Roslyn.Diagnostics.Analyzers.UnitTests
{
    public class InstanceIsNotCopyableTests
    {
        [Fact]
        public async Task ContainingTypeMustBeNonCopyable_CS()
        {
            var source = @"
struct CopyableContainer {
    NonCopyableType {|RS0040:_value|};
}

class NonCopyableReferenceContainer {
    NonCopyableType _value;
}

[NonCopyable]
struct NonCopyableValueContainer {
    NonCopyableType _value;
}

[NonCopyable]
struct NonCopyableType { }

class NonCopyableAttribute : System.Attribute { }
";

            await VerifyCS.VerifyAnalyzerAsync(source);
        }

        [Fact]
        public async Task ContainingTypeMustBeNonCopyable_VB()
        {
            var source = @"
Structure CopyableContainer
    Dim {|RS0040:_value|} As NonCopyableType
End Structure

Class NonCopyableReferenceContainer
    Dim _value As NonCopyableType
End Class

<NonCopyable>
Structure NonCopyableValueContainer
    Dim _value As NonCopyableType
End Structure

<NonCopyable>
Structure NonCopyableType
End Structure

Class NonCopyableAttribute
    Inherits System.Attribute
End Class
";

            await VerifyVB.VerifyAnalyzerAsync(source);
        }

        [Fact]
        public async Task PassParametersByReference()
        {
            var source = @"
class Class {
    void Method() {
        NonCopyableType value = default;
        Method1({|RS0039:value|});
        Method2(ref value);
        Method3(in value);
    }

    void Method1(NonCopyableType value) => throw null;
    void Method2(ref NonCopyableType value) => throw null;
    void Method3(in NonCopyableType value) => throw null;
}

[NonCopyable]
struct NonCopyableType { }

class NonCopyableAttribute : System.Attribute { }
";

            await VerifyCS.VerifyAnalyzerAsync(source);
        }

        [Fact]
        public async Task MustMoveToReturn()
        {
            var source = @"
class Class {
    NonCopyableType Method0() => new NonCopyableType();
    NonCopyableType Method1(ref NonCopyableType value) => {|RS0038:value|};
    NonCopyableType Method2(ref NonCopyableType value) => value.Move();
}

[NonCopyable]
struct NonCopyableType {
    public NonCopyableType Move() => throw null;
}

class NonCopyableAttribute : System.Attribute { }
";

            await VerifyCS.VerifyAnalyzerAsync(source);
        }

        [Fact]
        public async Task MustMoveToBox()
        {
            var source = @"
class Class {
    void Method(ref NonCopyableType value) {
        _ = $""{{|RS0038:value|}}"";
        _ = $""{value.Move()}"";

        object obj1 = {|RS0038:value|};
        object obj2 = value.Move();

        string str1 = {|RS0038:value|}.ToString();
        string str2 = value.Move().ToString();
    }
}

[NonCopyable]
struct NonCopyableType {
    public NonCopyableType Move() => throw null;
}

class NonCopyableAttribute : System.Attribute { }
";

            await VerifyCS.VerifyAnalyzerAsync(source);
        }
    }
}
