﻿// <auto-generated />
// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.ComponentModel;

namespace Roslyn.Utilities
{
    internal enum ContextDependency
    {
        /// <summary>
        /// The default dependency value. For synchronous operations and values, the default is <see cref="None"/>. For
        /// asynchronous operations and values, the default is <see cref="Any"/>.
        /// </summary>
        Default,

        /// <summary>
        /// The operation or value is not allowed to depend on the execution of the main thread to produce a result.
        /// </summary>
        None,

        /// <summary>
        /// The operation on value is only allowed to depend on the main thread to produce a result only when accessed
        /// from the main thread.
        /// </summary>
        Context,

        /// <summary>
        /// The operation or value is allowed to depend on the main thread to produce a result.
        /// </summary>
        Any,
    }

    [AttributeUsage(AttributeTargets.Field | AttributeTargets.Method | AttributeTargets.Parameter | AttributeTargets.Property | AttributeTargets.ReturnValue | AttributeTargets.GenericParameter, AllowMultiple = false, Inherited = false)]
    internal sealed class ThreadDependencyAttribute : Attribute
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="ThreadDependencyAttribute"/> class with the specified
        /// dependency type.
        /// </summary>
        /// <param name="contextDependency">The dependency type for the operation or value.</param>
        public ThreadDependencyAttribute(ContextDependency contextDependency)
        {
            ContextDependency = contextDependency;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="ThreadDependencyAttribute"/> class with the specified
        /// dependency type.
        /// </summary>
        /// <param name="contextDependency">The dependency type for the operation or value.</param>
        [EditorBrowsable(EditorBrowsableState.Never)]
        public ThreadDependencyAttribute(int contextDependency)
            : this((ContextDependency)contextDependency)
        {
        }

        /// <summary>
        /// Gets the dependency type for the operation or value.
        /// </summary>
        public ContextDependency ContextDependency { get; }

        /// <summary>
        /// <para>Gets or sets a value indicating whether the task is always completed.</para>
        /// <para>The default value is <see langword="false"/>.</para>
        /// </summary>
        public bool AlwaysCompleted { get; set; }

        /// <summary>
        /// <para>Gets or sets a value indicating whether the dependency claim applies only when the target instance of
        /// the operation is marked as not having a main thread dependency.</para>
        /// <para>The default value is <see langword="false"/>.</para>
        /// </summary>
        public bool PerInstance { get; set; }

        /// <summary>
        /// <para>Gets or sets a value indicating whether the dependency claim has been verified against the signatures
        /// and contracts of referenced code.</para>
        /// <para>The default value is <see langword="true"/>.</para>
        /// </summary>
        public bool Verified { get; set; } = true;
    }
}
