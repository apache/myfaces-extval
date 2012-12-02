/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.core.renderkit.exception;

import org.apache.myfaces.extensions.validator.core.interceptor.RendererInterceptor;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

/**
 * Exception that can be thrown by a
 * ({@link org.apache.myfaces.extensions.validator.core.interceptor.RendererInterceptor})
 * to skip the invocation of the intercepted renderer method.
 *
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public class SkipRendererDelegationException extends Exception
{
    protected RendererInterceptor exceptionSource;
    protected Object information;
    boolean skipOtherInterceptors = false;
    private static final long serialVersionUID = 2343074077532915722L;

    /**
     * Constructor for creating an exception which doesn't skip the invocation of the other interceptors.
     */
    public SkipRendererDelegationException()
    {
    }

    /**
     * Constructor for creating an exception which might skip the invocation of the other interceptors.
     *
     * @param skipOtherInterceptors signals if the other interceptors should be skipped.
     */
    public SkipRendererDelegationException(boolean skipOtherInterceptors)
    {
        this.skipOtherInterceptors = skipOtherInterceptors;
    }

    /**
     * Constructor for creating an exception which doesn't skip the invocation of the other interceptors.
     * Furthermore, it allows to get information about the {@link RendererInterceptor} which threw the exception.
     *
     * @param skipOtherInterceptors signals if the other interceptors should be skipped.
     * @param rendererInterceptor interceptor which threw this exception.
     */
    public SkipRendererDelegationException(boolean skipOtherInterceptors, RendererInterceptor rendererInterceptor)
    {
        this(skipOtherInterceptors);
        this.exceptionSource = rendererInterceptor;
    }

    /**
     * Sets an additional object with information.
     *
     * @param information some additional information.
     */
    public void setInformation(Object information)
    {
        this.information = information;
    }

    /**
     * Returns the additional information.
     *
     * @return the additional information.
     */
    public Object getInformation()
    {
        return information;
    }

    /**
     * Signals if the subsequent interceptors should be skipped.
     *
     * @return true if the subsequent interceptors should be skipped, false otherwise
     */
    public boolean isSkipOtherInterceptors()
    {
        return skipOtherInterceptors;
    }

    /**
     * Returns the value which should be used as result (if the intercepted method has to provide a return value).
     *
     * @param currentReturnValue the original converted value
     * @return value that should be returned as converted value.
     */
    public Object getReturnValueOnException(Object currentReturnValue)
    {
        if (this.exceptionSource != null)
        {
            return this.exceptionSource.getReturnValueOnSkipRendererDelegationException(this, currentReturnValue);
        }
        return currentReturnValue;
    }
}