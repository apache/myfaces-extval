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
 * Exception that can be thrown by RendererInterceptor implementations to indicate that the execution of the intercepted
 * renderer method is aborted.
 *
 * @author Gerhard Petracek
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
     * Initialization of empty Exception object with the skipOtherInterceptors property set to false. When exception is
     * thrown in the getConvertedValue method, the converted value can't be altered.
     */
    public SkipRendererDelegationException()
    {
    }

    /**
     * Initialization of an empty Exception object where we can specify it other interceptors should be skipped or not.
     * When exception is thrown in the getConvertedValue method, the converted value can't be altered.
     *
     * @param skipOtherInterceptors should other interceptors be skipped.
     */
    public SkipRendererDelegationException(boolean skipOtherInterceptors)
    {
        this.skipOtherInterceptors = skipOtherInterceptors;
    }

    /**
     * Initialization of an Exception where we supply the interceptor where it went wrong.  This interceptor is then
     * consulted for the final value of the converted value.
     *
     * @param skipOtherInterceptors should other interceptors be skipped.
     * @param rendererInterceptor interceptor that is the cause of this exception.
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
     * Gets the additional information.
     *
     * @return the additional information.
     */
    public Object getInformation()
    {
        return information;
    }

    /**
     * Should other interceptors be skipped according to the info supplied at the time of creation of the exception.
     *
     * @return should other interceptors be skipped.
     */
    public boolean isSkipOtherInterceptors()
    {
        return skipOtherInterceptors;
    }

    /**
     * When a rendererInterceptor is supplied at the time of the creation, it is consulted about the value the ExtVal
     * system should return as converted value.  Otherwise, it is the value of the parameter.
     *
     * @param currentReturnValue Converted value defined by the JSF system.
     * @return Converted value that we should return.
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
