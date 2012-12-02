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
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipRendererDelegationException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipBeforeInterceptorsException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipAfterInterceptorsException;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.convert.ConverterException;
import javax.faces.render.Renderer;
import java.io.IOException;
import java.util.logging.Logger;

/**
 * Empty base implementation which allows concrete implementations to override just the needed methods.
 *
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public abstract class AbstractRendererInterceptor implements RendererInterceptor
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    protected AbstractRendererInterceptor()
    {
            logger.fine(getClass().getName() + " instantiated");
    }

    public final String getInterceptorId()
    {
        return getClass().getName();
    }

    /**
     * {@inheritDoc}
     */
    public Object getReturnValueOnSkipRendererDelegationException(
        SkipRendererDelegationException skipRendererDelegationException, Object currentReturnValue)
    {
        return currentReturnValue;
    }

    /*
    * before
    */
    /**
     * {@inheritDoc}
     */
    public void beforeDecode(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
        throws SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
    }

    /**
     * {@inheritDoc}
     */
    public void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
        throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
    }

    /**
     * {@inheritDoc}
     */
    public void beforeEncodeChildren(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
        throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
    }

    /**
     * {@inheritDoc}
     */
    public void beforeEncodeEnd(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
        throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
    }

    /**
     * {@inheritDoc}
     */
    public void beforeGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o, Renderer wrapped)
        throws ConverterException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
    }

    /*
     * after
     */
    /**
     * {@inheritDoc}
     */
    public void afterDecode(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
        throws SkipAfterInterceptorsException
    {
    }

    /**
     * {@inheritDoc}
     */
    public void afterEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
        throws IOException, SkipAfterInterceptorsException
    {
    }

    /**
     * {@inheritDoc}
     */
    public void afterEncodeChildren(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
        throws IOException, SkipAfterInterceptorsException
    {
    }

    /**
     * {@inheritDoc}
     */
    public void afterEncodeEnd(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
        throws IOException, SkipAfterInterceptorsException
    {
    }

    /**
     * {@inheritDoc}
     */
    public void afterGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o, Renderer wrapped)
        throws ConverterException, SkipAfterInterceptorsException
    {
    }
}