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
package org.apache.myfaces.extensions.validator.core.interceptor;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipBeforeInterceptorsException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipRendererDelegationException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipAfterInterceptorsException;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.convert.ConverterException;
import javax.faces.render.Renderer;
import java.io.IOException;

/**
 * Allows to intercept renderer methods.<br/>
 * Base mechanism of extval. It allows to add custom infrastructures.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public interface RendererInterceptor
{
    String getInterceptorId();

    /**
     * Defines the value for the converted value that should be returned by the getConvertedValue method of the
     * Renderer in case the there was a SkipRendererDelegationException thrown.
     * @param skipRendererDelegationException The exception thrown to abort RendererInterceptor execution.
     * @param currentReturnValue The converted value that is defined at this time.  This can be null also when the
     * user has inputted some text in case of an Exception during the beforeConvertedValue 'phase'
     * @return value that should be used as converted value.
     */
    Object getReturnValueOnSkipRendererDelegationException(
        SkipRendererDelegationException skipRendererDelegationException, Object currentReturnValue);

    /*
     * before
     */

    /**
     * The "before decode phase" of ExtVal.  The code is executed before the decode method is executed of the Renderer.
     * @param facesContext The JSF Context
     * @param uiComponent The component which is processed
     * @param renderer The renderer that will be called for the apply request values JSF phase.
     * @throws SkipBeforeInterceptorsException Can be thrown to stop the execution of the beforeDecode methods of the
     * registered interceptors.
     * @throws SkipRendererDelegationException Can be thrown to stop the execution of the beforeDecode method and allows
     *  additional interceptors to run and change the converted value.
     */
    void beforeDecode(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws SkipBeforeInterceptorsException, SkipRendererDelegationException;

    /**
     * The "before encode phase" of ExtVal.  The code is executed before the encodeBegin method is executed of the
     * Renderer.
     * @param facesContext The JSF Context
     * @param uiComponent The component which is processed
     * @param renderer The renderer that will be called for the render response JSF phase.
     * @throws IOException  In case the response writer is accessed and there was an IO problem.
     * @throws SkipBeforeInterceptorsException Can be thrown to stop the execution of the beforeEncodeBegin methods of
     * the registered interceptors.
     * @throws SkipRendererDelegationException Can be thrown to stop the execution of the beforeEncodeBegin method.
     */
    void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException;

    /**
     * The "before encode children phase" of ExtVal.  The code is executed before the encodeChildren method is executed
     * of the Renderer.
     * @param facesContext The JSF Context
     * @param uiComponent  The component which is processed
     * @param renderer The renderer that will be called for the render response JSF phase.
     * @throws IOException In case the response writer is accessed and there was an IO problem.
     * @throws SkipBeforeInterceptorsException Can be thrown to stop the execution of the beforeEncodeChildren methods
     * of the registered interceptors.
     * @throws SkipRendererDelegationException Can be thrown to stop the execution of the beforeEncodeChildren method.
     */
    void beforeEncodeChildren(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException;

    /**
     * The "before encode end phase" of ExtVal.  The code is executed before the encodeEnd method is executed
     * of the Renderer.
     *
     * @param facesContext The JSF Context
     * @param uiComponent  The component which is processed
     * @param renderer The renderer that will be called for the render response JSF phase.
     * @throws IOException In case the response writer is accessed and there was an IO problem.
     * @throws SkipBeforeInterceptorsException Can be thrown to stop the execution of the beforeEncodeEnd methods of the
     * registered interceptors.
     * @throws SkipRendererDelegationException Can be thrown to stop the execution of the beforeEncodeEnd method.
     */
    void beforeEncodeEnd(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException;

    /**
     * The "before get converted value phase" of ExtVal.  The code is executed before the getConvertedValue method is
     * executed of the Renderer.
     *
     * @param facesContext The JSF Context
     * @param uiComponent  The component which is processed
     * @param submittedValue The submitted value
     * @param renderer The renderer that will be called for the apply request values JSF phase.
     * @throws ConverterException ExtVal validation strategies can throw ValidationExceptions which are converted by
     * AbstractValidationInterceptor
     * @throws SkipBeforeInterceptorsException Can be thrown to stop the execution of the beforeGetConvertedValue
     * methods of the registered interceptors.
     * @throws SkipRendererDelegationException Can be thrown to stop the execution of the beforeGetConvertedValue
     * method.
     */
    void beforeGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object submittedValue,
                                 Renderer renderer)
        throws ConverterException, SkipBeforeInterceptorsException, SkipRendererDelegationException;

    /*
     * after
     */

    /**
     * The "after decode phase" of ExtVal.  The code is executed after the decode method is executed
     * of the Renderer.
     *
     * @param facesContext The JSF Context
     * @param uiComponent  The component which is processed
     * @param renderer The renderer that will be called for the apply request values JSF phase.
     * @throws SkipAfterInterceptorsException Can be thrown to stop the execution of the afterDecode
     * methods of the registered interceptors.
     */
    void afterDecode(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws SkipAfterInterceptorsException;

    /**
     * The "after encode begin phase" of ExtVal.  The code is executed after the encodeBegin method is executed
     * of the Renderer.
     *
     * @param facesContext The JSF context
     * @param uiComponent In case the response writer is accessed and there was an IO problem.
     * @param renderer The renderer that will be called for the render response JSF phase.
     * @throws IOException In case the response writer is accessed and there was an IO problem.
     * @throws SkipAfterInterceptorsException Can be thrown to stop the execution of the afterEncodeBegin
     * methods of the registered interceptors.
     */
    void afterEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException, SkipAfterInterceptorsException;

    /**
     * The "after encode children phase" of ExtVal.  The code is executed after the encodeChildren method is executed
     * of the Renderer.
     *
     * @param facesContext The JSF Context
     * @param uiComponent  The component which is processed
     * @param renderer The renderer that will be called for the render response JSF phase.
     * @throws IOException In case the response writer is accessed and there was an IO problem.
     * @throws SkipAfterInterceptorsException Can be thrown to stop the execution of the afterEncodeChildren
     * methods of the registered interceptors.
     */
    void afterEncodeChildren(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException, SkipAfterInterceptorsException;

    /**
     * The "after encode end phase" of ExtVal.  The code is executed after the encodeEnd method is executed
     * of the Renderer.
     *
     * @param facesContext The JSF Context
     * @param uiComponent  The component which is processed
     * @param renderer The renderer that will be called for the render response JSF phase.
     * @throws IOException In case the response writer is accessed and there was an IO problem.
     * @throws SkipAfterInterceptorsException Can be thrown to stop the execution of the afterEncodeEnd
     * methods of the registered interceptors.
     */
    void afterEncodeEnd(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException, SkipAfterInterceptorsException;

    /**
     * The "after get converted value phase" of ExtVal.  The code is executed after the getConvertedValue method is
     * executed of the Renderer.
     *
     * @param facesContext The JSF Context
     * @param uiComponent  The component which is processed
     * @param submittedValue The submitted value
     * @param renderer The renderer that will be called for the render response JSF phase.
     * @throws ConverterException ExtVal validation strategies can throw ValidationExceptions which are converted by
     * AbstractValidationInterceptor
     * @throws SkipAfterInterceptorsException Can be thrown to stop the execution of the afterGetConvertedValue
     * methods of the registered interceptors.
     */
    void afterGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object submittedValue, Renderer
            renderer)
        throws ConverterException, SkipAfterInterceptorsException;
}
