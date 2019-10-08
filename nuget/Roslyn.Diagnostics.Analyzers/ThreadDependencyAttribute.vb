﻿' <auto-generated />
' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports System

Namespace Global.Roslyn.Utilities
    Friend Module ContextDependency
        ''' <summary>
        ''' The default dependency value. For synchronous operations and values, the default is <see cref="None"/>. For
        ''' asynchronous operations and values, the default is <see cref="Any"/>.
        ''' </summary>
        Public Const [Default] As Integer = 0

        ''' <summary>
        ''' The operation or value is not allowed to depend on the execution of the main thread to produce a result.
        ''' </summary>
        Public Const None As Integer = 1

        ''' <summary>
        ''' The operation on value is only allowed to depend on the main thread to produce a result only when accessed
        ''' from the main thread.
        ''' </summary>
        Public Const Context As Integer = 2

        ''' <summary>
        ''' The operation or value is allowed to depend on the main thread to produce a result.
        ''' </summary>
        Public Const Any As Integer = 3
    End Module

    <AttributeUsage(AttributeTargets.Field Or AttributeTargets.Method Or AttributeTargets.Parameter Or AttributeTargets.Property Or AttributeTargets.ReturnValue, AllowMultiple:=False, Inherited:=True)>
    Friend NotInheritable Class ThreadDependencyAttribute
        Inherits Attribute

        ''' <summary>
        ''' Initializes a new instance of the <see cref="ThreadDependencyAttribute"/> class with the specified
        ''' dependency type.
        ''' </summary>
        ''' <param name="contextDependency">The dependency type for the operation or value.</param>
        Public Sub New(contextDependency As Integer)
            Me.ContextDependency = contextDependency
        End Sub

        ''' <summary>
        ''' Gets the dependency type for the operation or value.
        ''' </summary>
        Public ReadOnly Property ContextDependency As Integer

        ''' <summary>
        ''' <para>Gets or sets a value indicating whether the task is always completed.</para>
        ''' <para>The default value is <see langword="False"/>.</para>
        ''' </summary>
        Public Property AlwaysCompleted As Boolean

        ''' <summary>
        ''' <para>Gets or sets a value indicating whether the dependency claim further depends on the current context at
        ''' method entry. Such operations only have no main thread dependency if they are invoked from a background
        ''' thread.</para>
        ''' <para>The default value is <see langword="False"/>.</para>
        ''' </summary>
        Public Property CapturesContext As Boolean

        ''' <summary>
        ''' <para>Gets or sets a value indicating whether the dependency claim applies only when the target instance of
        ''' the operation is marked as not having a main thread dependency.</para>
        ''' <para>The default value is <see langword="False"/>.</para>
        ''' </summary>
        Public Property PerInstance As Boolean

        ''' <summary>
        ''' <para>Gets or sets a value indicating whether the dependency claim has been verified against the signatures
        ''' and contracts of referenced code.</para>
        ''' <para>The default value is <see langword="True"/>.</para>
        ''' </summary>
        Public Property Verified As Boolean = True
    End Class
End Namespace