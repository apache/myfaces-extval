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
 * It's the base mechanism of ExtVal which enables most of the concepts provided by the framework.
 * Furthermore, it allows to add custom concepts.
 *
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public interface RendererInterceptor
{
    String getInterceptorId();

    /**
     * In case of the converted value it defines the value that should be returned by the getConvertedValue method
     * in case there was a {@link SkipRendererDelegationException}.
     *
     * @param skipRendererDelegationException The exception thrown to abort further {@link RendererInterceptor}s.
     * @param currentReturnValue The converted value that is defined at this time.
     * @return value that should be used as converted value.
     */
    Object getReturnValueOnSkipRendererDelegationException(
        SkipRendererDelegationException skipRendererDelegationException, Object currentReturnValue);

    /*
     * before
     */

    /**
     * Intercepts a method of the renderer. The name of the intercepted method is the name of this method without the
     * prefix ('before').
     *  
     * @param facesContext The JSF Context
     * @param uiComponent The current component
     * @param renderer The intercepted renderer
     * @throws SkipBeforeInterceptorsException can be thrown to stop the execution of the subsequent interceptors
     * @throws SkipRendererDelegationException can be thorwn to skip the invocation of the intercepted renderer method.
     */
    void beforeDecode(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws SkipBeforeInterceptorsException, SkipRendererDelegationException;

    /**
     * Intercepts a method of the renderer. The name of the intercepted method is the name of this method without the
     * prefix ('before').
     *
     * @param facesContext The JSF Context
     * @param uiComponent The current component
     * @param renderer The intercepted renderer
     * @throws IOException  In case the response writer is accessed and there was an IO problem.
     * @throws SkipBeforeInterceptorsException can be thrown to stop the execution of the subsequent interceptors
     * @throws SkipRendererDelegationException can be thorwn to skip the invocation of the intercepted renderer method.
     */
    void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException;

    /**
     * Intercepts a method of the renderer. The name of the intercepted method is the name of this method without the
     * prefix ('before').
     *
     * @param facesContext The JSF Context
     * @param uiComponent The current component
     * @param renderer The intercepted renderer
     * @throws IOException In case the response writer is accessed and there was an IO problem.
     * @throws SkipBeforeInterceptorsException can be thrown to stop the execution of the subsequent interceptors
     * @throws SkipRendererDelegationException can be thorwn to skip the invocation of the intercepted renderer method.
     */
    void beforeEncodeChildren(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException;

    /**
     * Intercepts a method of the renderer. The name of the intercepted method is the name of this method without the
     * prefix ('before').
     *
     * @param facesContext The JSF Context
     * @param uiComponent The current component
     * @param renderer The intercepted renderer
     * @throws IOException In case the response writer is accessed and there was an IO problem.
     * @throws SkipBeforeInterceptorsException can be thrown to stop the execution of the subsequent interceptors
     * @throws SkipRendererDelegationException can be thorwn to skip the invocation of the intercepted renderer method.
     */
    void beforeEncodeEnd(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException;

    /**
     * Intercepts a method of the renderer. The name of the intercepted method is the name of this method without the
     * prefix ('before').
     *
     * @param facesContext The JSF Context
     * @param uiComponent The current component
     * @param submittedValue The submitted value
     * @param renderer The intercepted renderer
     * @throws ConverterException ExtVal validation strategies can throw
     * {@link javax.faces.validator.ValidatorException}s.
     * Due to the trick used by ExtVal it has to be converted to a {@link ConverterException}
     * (see {@link AbstractValidationInterceptor}).
     * @throws SkipBeforeInterceptorsException can be thrown to stop the execution of the subsequent interceptors
     * @throws SkipRendererDelegationException can be thorwn to skip the invocation of the intercepted renderer method.
     */
    void beforeGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object submittedValue,
                                 Renderer renderer)
        throws ConverterException, SkipBeforeInterceptorsException, SkipRendererDelegationException;

    /*
     * after
     */

    /**
     * Intercepts a method of the renderer. The name of the intercepted method is the name of this method without the
     * prefix ('after').
     *
     * @param facesContext The JSF Context
     * @param uiComponent The current component
     * @param renderer The intercepted renderer
     * @throws SkipAfterInterceptorsException Can be thrown to stop the execution of the subsequent interceptors.
     */
    void afterDecode(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws SkipAfterInterceptorsException;

    /**
     * Intercepts a method of the renderer. The name of the intercepted method is the name of this method without the
     * prefix ('after').
     *
     * @param facesContext The JSF context
     * @param uiComponent The current component
     * @param renderer The intercepted renderer
     * @throws IOException In case the response writer is accessed and there was an IO problem.
     * @throws SkipAfterInterceptorsException Can be thrown to stop the execution of the subsequent interceptors.
     */
    void afterEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException, SkipAfterInterceptorsException;

    /**
     * Intercepts a method of the renderer. The name of the intercepted method is the name of this method without the
     * prefix ('after').
     *
     * @param facesContext The JSF Context
     * @param uiComponent The current component
     * @param renderer The intercepted renderer
     * @throws IOException In case the response writer is accessed and there was an IO problem.
     * @throws SkipAfterInterceptorsException Can be thrown to stop the execution of the subsequent interceptors.
     */
    void afterEncodeChildren(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException, SkipAfterInterceptorsException;

    /**
     * Intercepts a method of the renderer. The name of the intercepted method is the name of this method without the
     * prefix ('after').
     *
     * @param facesContext The JSF Context
     * @param uiComponent The current component
     * @param renderer The intercepted renderer
     * @throws IOException In case the response writer is accessed and there was an IO problem.
     * @throws SkipAfterInterceptorsException Can be thrown to stop the execution of the subsequent interceptors.
     */
    void afterEncodeEnd(FacesContext facesContext, UIComponent uiComponent, Renderer renderer)
        throws IOException, SkipAfterInterceptorsException;

    /**
     * Intercepts a method of the renderer. The name of the intercepted method is the name of this method without the
     * prefix ('after').
     *
     * @param facesContext The JSF Context
     * @param uiComponent The current component
     * @param submittedValue The submitted value
     * @param renderer The intercepted renderer
     * @throws ConverterException ExtVal validation strategies can throw
     * {@link javax.faces.validator.ValidatorException}s.
     * Due to the trick used by ExtVal it has to be converted to a {@link ConverterException}
     * (see {@link AbstractValidationInterceptor}).
     * @throws SkipAfterInterceptorsException Can be thrown to stop the execution of the subsequent interceptors.
     */
    void afterGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object submittedValue, Renderer
            renderer)
        throws ConverterException, SkipAfterInterceptorsException;
}
