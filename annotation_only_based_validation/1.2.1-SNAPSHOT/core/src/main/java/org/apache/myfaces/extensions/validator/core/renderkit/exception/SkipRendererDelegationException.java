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

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
public class SkipRendererDelegationException extends Exception
{
    protected RendererInterceptor exceptionSource;
    protected Object information;
    boolean skipOtherInterceptors = false;

    public SkipRendererDelegationException()
    {
    }

    public SkipRendererDelegationException(boolean skipOtherInterceptors)
    {
        this.skipOtherInterceptors = skipOtherInterceptors;
    }

    public SkipRendererDelegationException(boolean skipOtherInterceptors, RendererInterceptor rendererInterceptor)
    {
        this(skipOtherInterceptors);
        this.exceptionSource = rendererInterceptor;
    }

    public void setInformation(Object information)
    {
        this.information = information;
    }

    public Object getInformation()
    {
        return information;
    }

    public boolean isSkipOtherInterceptors()
    {
        return skipOtherInterceptors;
    }

    public Object getReturnValueOnException(Object currentReturnValue)
    {
        if(this.exceptionSource != null)
        {
            return this.exceptionSource.getReturnValueOnSkipRendererDelegationException(this, currentReturnValue);
        }
        return currentReturnValue;
    }
}