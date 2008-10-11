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
package org.apache.myfaces.extensions.validator.core.renderkit;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.core.interceptor.RendererInterceptor;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipBeforeInterceptorsException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipAfterInterceptorsException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipRendererDelegationException;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.logging.Log;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.ConverterException;
import javax.faces.render.Renderer;
import java.io.IOException;

/**
 * Default approach to avoid proxies for converters and the adapter fallback.
 * It requires that components delegate getConvertedValue to a renderer.<br/>
 * If it isn't the case for your component lib use:
 * org.apache.myfaces.extensions.validator.core.proxy.ExtValApplicationFactory<br/>
 * and<br/>
 * org.apache.myfaces.extensions.validator.core.proxy.ProxyMappingPhaseListener
 * <p/>
 * This wrapper will also implement client-side validation behaviour
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValRendererWrapper extends Renderer
{
    protected final Log logger = LogFactory.getLog(getClass());

    protected Renderer wrapped;
    protected ExtValContext extValContext = ExtValContext.getContext();

    public ExtValRendererWrapper(Renderer renderer)
    {
        this.wrapped = new ExtValRendererProxy(renderer);

        if(logger.isTraceEnabled())
        {
            logger.trace("extval renderer wrapper created for " + renderer.getClass().getName());
        }
    }

    @Override
    public final void decode(FacesContext facesContext, UIComponent uiComponent)
    {
        boolean delegateToWrappedRenderer = true;

        try
        {
            for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
            {
                if(logger.isTraceEnabled())
                {
                    logger.trace("start beforeDecode of " + rendererInterceptor.getClass().getName());
                }

                try
                {
                    rendererInterceptor.beforeDecode(facesContext, uiComponent, this.wrapped);
                }
                catch (SkipRendererDelegationException e)
                {
                    if(logger.isTraceEnabled())
                    {
                        logger.trace("decode delegation canceled", e);
                    }

                    delegateToWrappedRenderer = false;

                    if(e.isSkipOtherInterceptors())
                    {
                        break;
                    }
                }

                if(logger.isTraceEnabled())
                {
                    logger.trace("beforeDecode of " + rendererInterceptor.getClass().getName() + " finished");
                }
            }
        }
        catch(SkipBeforeInterceptorsException e)
        {
            if(logger.isTraceEnabled())
            {
                logger.trace("beforeDecode interceptors canceled", e);
            }
        }

        /*
         * delegate
         */
        if(delegateToWrappedRenderer)
        {
            wrapped.decode(facesContext, uiComponent);
        }

        try
        {
            for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
            {
                if(logger.isTraceEnabled())
                {
                    logger.trace("start afterDecode of " + rendererInterceptor.getClass().getName());
                }

                rendererInterceptor.afterDecode(facesContext, uiComponent, this.wrapped);

                if(logger.isTraceEnabled())
                {
                    logger.trace("afterDecode of " + rendererInterceptor.getClass().getName() + " finished");
                }
            }
        }
        catch (SkipAfterInterceptorsException e)
        {
            if(logger.isTraceEnabled())
            {
                logger.trace("afterDecode interceptors canceled", e);
            }
        }
    }

    @Override
    public final void encodeBegin(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        boolean delegateToWrappedRenderer = true;

        try
        {
            for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
            {
                if(logger.isTraceEnabled())
                {
                    logger.trace("start beforeEncodeBegin of " + rendererInterceptor.getClass().getName());
                }

                try
                {
                    rendererInterceptor.beforeEncodeBegin(facesContext, uiComponent, this.wrapped);
                }
                catch (SkipRendererDelegationException e)
                {
                    if(logger.isTraceEnabled())
                    {
                        logger.trace("encodeBegin delegation canceled", e);
                    }

                    delegateToWrappedRenderer = false;

                    if(e.isSkipOtherInterceptors())
                    {
                        break;
                    }
                }

                if(logger.isTraceEnabled())
                {
                    logger.trace("beforeEncodeBegin of " + rendererInterceptor.getClass().getName() + " finished");
                }
            }
        }
        catch (SkipBeforeInterceptorsException e)
        {
            if(logger.isTraceEnabled())
            {
                logger.trace("beforeEncodeBegin interceptors canceled", e);
            }
        }

        /*
         * delegate
         */
        if(delegateToWrappedRenderer)
        {
            wrapped.encodeBegin(facesContext, uiComponent);
        }

        try
        {
            for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
            {
                if(logger.isTraceEnabled())
                {
                    logger.trace("start afterEncodeBegin of " + rendererInterceptor.getClass().getName());
                }

                    rendererInterceptor.afterEncodeBegin(facesContext, uiComponent, this.wrapped);

                if(logger.isTraceEnabled())
                {
                    logger.trace("afterEncodeBegin of " + rendererInterceptor.getClass().getName() + " finished");
                }
            }
        }
        catch (SkipAfterInterceptorsException e)
        {
            if(logger.isTraceEnabled())
            {
                logger.trace("afterEncodeBegin interceptors canceled", e);
            }
        }
    }

    @Override
    public final void encodeChildren(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        boolean delegateToWrappedRenderer = true;

        try
        {
            for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
            {
                if(logger.isTraceEnabled())
                {
                    logger.trace("start beforeEncodeChildren of " + rendererInterceptor.getClass().getName());
                }

                try
                {
                    rendererInterceptor.beforeEncodeChildren(facesContext, uiComponent, this.wrapped);
                }
                catch (SkipRendererDelegationException e)
                {
                    if(logger.isTraceEnabled())
                    {
                        logger.trace("encodeChildren delegation canceled", e);
                    }

                    delegateToWrappedRenderer = false;

                    if(e.isSkipOtherInterceptors())
                    {
                        break;
                    }
                }

                if(logger.isTraceEnabled())
                {
                    logger.trace("beforeEncodeChildren of " +
                        rendererInterceptor.getClass().getName() + " finished");
                }
            }
        }
        catch (SkipBeforeInterceptorsException e)
        {
            if(logger.isTraceEnabled())
            {
                logger.trace("beforeEncodeChildren interceptors canceled", e);
            }
        }

        /*
         * delegate
         */
        if(delegateToWrappedRenderer)
        {
            wrapped.encodeChildren(facesContext, uiComponent);
        }

        try
        {
            for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
            {
                if(logger.isTraceEnabled())
                {
                    logger.trace("start afterEncodeChildren of " + rendererInterceptor.getClass().getName());
                }

                rendererInterceptor.afterEncodeChildren(facesContext, uiComponent, this.wrapped);

                if(logger.isTraceEnabled())
                {
                    logger.trace("afterEncodeChildren of " + rendererInterceptor.getClass().getName() + " finished");
                }
            }
        }
        catch (SkipAfterInterceptorsException e)
        {
            if(logger.isTraceEnabled())
            {
                logger.trace("afterEncodeChildren interceptors canceled", e);
            }
        }
    }

    @Override
    public final void encodeEnd(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        boolean delegateToWrappedRenderer = true;

        try
        {
            for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
            {
                if(logger.isTraceEnabled())
                {
                    logger.trace("start beforeEncodeEnd of " + rendererInterceptor.getClass().getName());
                }

                try
                {
                    rendererInterceptor.beforeEncodeEnd(facesContext, uiComponent, this.wrapped);
                }
                catch (SkipRendererDelegationException e)
                {
                    if(logger.isTraceEnabled())
                    {
                        logger.trace("encodeEnd delegation canceled", e);
                    }

                    delegateToWrappedRenderer = false;

                    if(e.isSkipOtherInterceptors())
                    {
                        break;
                    }
                }

                if(logger.isTraceEnabled())
                {
                    logger.trace("beforeEncodeEnd of " + rendererInterceptor.getClass().getName() + " finished");
                }
            }
        }
        catch (SkipBeforeInterceptorsException e)
        {
            if(logger.isTraceEnabled())
            {
                logger.trace("beforeEncodeEnd interceptors canceled", e);
            }
        }

        /*
         * delegate
         */
        if(delegateToWrappedRenderer)
        {
            wrapped.encodeEnd(facesContext, uiComponent);
        }

        try
        {
            for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
            {
                if(logger.isTraceEnabled())
                {
                    logger.trace("start afterEncodeEnd of " + rendererInterceptor.getClass().getName());
                }

                rendererInterceptor.afterEncodeEnd(facesContext, uiComponent, this.wrapped);

                if(logger.isTraceEnabled())
                {
                    logger.trace("afterEncodeEnd of " + rendererInterceptor.getClass().getName() + " finished");
                }
            }
        }
        catch (SkipAfterInterceptorsException e)
        {
            if(logger.isTraceEnabled())
            {
                logger.trace("afterEncodeEnd interceptors canceled", e);
            }
        }
    }

    @Override
    public final String convertClientId(FacesContext facesContext, String s)
    {
        return wrapped.convertClientId(facesContext, s);
    }

    @Override
    public final boolean getRendersChildren()
    {
        return wrapped.getRendersChildren();
    }

    @Override
    public final Object getConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o)
        throws ConverterException
    {
        boolean delegateToWrappedRenderer = true;
        Object convertedObject = null;

        try
        {
            for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
            {
                if(logger.isTraceEnabled())
                {
                    logger.trace("start beforeGetConvertedValue of " + rendererInterceptor.getClass().getName());
                }

                try
                {
                    rendererInterceptor.beforeGetConvertedValue(facesContext, uiComponent, o, this.wrapped);
                }
                catch (SkipRendererDelegationException e)
                {
                    convertedObject = e.getReturnValueOnException(convertedObject);

                    if(logger.isTraceEnabled())
                    {
                        logger.trace("getConvertedValue delegation canceled", e);
                    }

                    delegateToWrappedRenderer = false;

                    if(e.isSkipOtherInterceptors())
                    {
                        break;
                    }
                }

                if(logger.isTraceEnabled())
                {
                    logger.trace("beforeGetConvertedValue of " +
                        rendererInterceptor.getClass().getName() + " finished");
                }
            }
        }
        catch (SkipBeforeInterceptorsException e)
        {
            if(logger.isTraceEnabled())
            {
                logger.trace("beforeGetConvertedValue interceptors canceled", e);
            }
        }

        /*
         * delegate
         */
        if(delegateToWrappedRenderer)
        {
            convertedObject = wrapped.getConvertedValue(facesContext, uiComponent, o);
        }

        try
        {
            for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
            {
                if(logger.isTraceEnabled())
                {
                    logger.trace("start afterGetConvertedValue of " + rendererInterceptor.getClass().getName());
                }

                rendererInterceptor.afterGetConvertedValue(facesContext, uiComponent, o, this.wrapped);

                if(logger.isTraceEnabled())
                {
                    logger.trace("afterGetConvertedValue of " + rendererInterceptor.getClass().getName() + " finished");
                }
            }
        }
        catch (SkipAfterInterceptorsException e)
        {
            if(logger.isTraceEnabled())
            {
                logger.trace("afterGetConvertedValue interceptors canceled", e);
            }
        }

        return convertedObject;
    }
}
