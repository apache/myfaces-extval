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
import org.apache.myfaces.extensions.validator.util.ClassUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.ConverterException;
import javax.faces.render.Renderer;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.util.logging.Logger;
import java.util.logging.Level;

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
    protected final Logger logger = Logger.getLogger(getClass().getName());

    protected Renderer wrapped;
    protected ExtValContext extValContext = ExtValContext.getContext();

    public ExtValRendererWrapper(Renderer renderer)
    {
        String proxyClassName = (String)ExtValContext.getContext().getGlobalProperty(ExtValRendererProxy.KEY);

        if(proxyClassName == null)
        {
            logger.finest("no extval renderer proxy configured");

            this.wrapped = new ExtValLazyRendererProxy(renderer);
            return;
        }

        Class targetClass = ClassUtils.tryToLoadClassForName(proxyClassName);

        if(targetClass == null)
        {
            logger.finest("no extval renderer proxy configured");

            this.wrapped = new ExtValLazyRendererProxy(renderer);
            return;
        }

        Class[] argClasses = new Class[1];
        argClasses[0] = Renderer.class;

        try
        {
            Constructor constructor = targetClass.getConstructor(argClasses);
            this.wrapped = (Renderer)constructor.newInstance(renderer);
        }
        catch (Throwable t)
        {
            logger.warning("no extval renderer proxy created for " + renderer.getClass().getName());
        }

        logger.finest("extval renderer wrapper created for " + renderer.getClass().getName());
    }

    @Override
    public final void decode(FacesContext facesContext, UIComponent uiComponent)
    {
        boolean delegateToWrappedRenderer = true;

        try
        {
            for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
            {
                logger.finest("start beforeDecode of " + rendererInterceptor.getClass().getName());

                try
                {
                    rendererInterceptor.beforeDecode(facesContext, uiComponent, this.wrapped);
                }
                catch (SkipRendererDelegationException e)
                {
                    logger.log(Level.FINEST, "decode delegation canceled", e);

                    delegateToWrappedRenderer = false;

                    if(e.isSkipOtherInterceptors())
                    {
                        break;
                    }
                }

                logger.finest("beforeDecode of " + rendererInterceptor.getClass().getName() + " finished");
            }
        }
        catch(SkipBeforeInterceptorsException e)
        {
            logger.log(Level.FINEST, "beforeDecode interceptors canceled", e);
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
                logger.finest("start afterDecode of " + rendererInterceptor.getClass().getName());

                rendererInterceptor.afterDecode(facesContext, uiComponent, this.wrapped);

                logger.finest("afterDecode of " + rendererInterceptor.getClass().getName() + " finished");
            }
        }
        catch (SkipAfterInterceptorsException e)
        {
            logger.log(Level.FINEST, "afterDecode interceptors canceled", e);
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
                logger.finest("start beforeEncodeBegin of " + rendererInterceptor.getClass().getName());

                try
                {
                    rendererInterceptor.beforeEncodeBegin(facesContext, uiComponent, this.wrapped);
                }
                catch (SkipRendererDelegationException e)
                {
                    logger.log(Level.FINEST, "encodeBegin delegation canceled", e);

                    delegateToWrappedRenderer = false;

                    if(e.isSkipOtherInterceptors())
                    {
                        break;
                    }
                }

                logger.finest("beforeEncodeBegin of " + rendererInterceptor.getClass().getName() + " finished");
            }
        }
        catch (SkipBeforeInterceptorsException e)
        {
            logger.log(Level.FINEST, "beforeEncodeBegin interceptors canceled", e);
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
                logger.finest("start afterEncodeBegin of " + rendererInterceptor.getClass().getName());

                rendererInterceptor.afterEncodeBegin(facesContext, uiComponent, this.wrapped);

                logger.finest("afterEncodeBegin of " + rendererInterceptor.getClass().getName() + " finished");
            }
        }
        catch (SkipAfterInterceptorsException e)
        {
            logger.log(Level.FINEST, "afterEncodeBegin interceptors canceled", e);
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
                logger.finest("start beforeEncodeChildren of " + rendererInterceptor.getClass().getName());

                try
                {
                    rendererInterceptor.beforeEncodeChildren(facesContext, uiComponent, this.wrapped);
                }
                catch (SkipRendererDelegationException e)
                {
                    logger.log(Level.FINEST, "encodeChildren delegation canceled", e);

                    delegateToWrappedRenderer = false;

                    if(e.isSkipOtherInterceptors())
                    {
                        break;
                    }
                }

                logger.finest("beforeEncodeChildren of " +
                    rendererInterceptor.getClass().getName() + " finished");
            }
        }
        catch (SkipBeforeInterceptorsException e)
        {
            logger.log(Level.FINEST, "beforeEncodeChildren interceptors canceled", e);
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
                logger.finest("start afterEncodeChildren of " + rendererInterceptor.getClass().getName());

                rendererInterceptor.afterEncodeChildren(facesContext, uiComponent, this.wrapped);

                logger.finest("afterEncodeChildren of " + rendererInterceptor.getClass().getName() + " finished");
            }
        }
        catch (SkipAfterInterceptorsException e)
        {
            logger.log(Level.FINEST, "afterEncodeChildren interceptors canceled", e);
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
                logger.finest("start beforeEncodeEnd of " + rendererInterceptor.getClass().getName());

                try
                {
                    rendererInterceptor.beforeEncodeEnd(facesContext, uiComponent, this.wrapped);
                }
                catch (SkipRendererDelegationException e)
                {
                    logger.log(Level.FINEST, "encodeEnd delegation canceled", e);

                    delegateToWrappedRenderer = false;

                    if(e.isSkipOtherInterceptors())
                    {
                        break;
                    }
                }

                logger.finest("beforeEncodeEnd of " + rendererInterceptor.getClass().getName() + " finished");
            }
        }
        catch (SkipBeforeInterceptorsException e)
        {
            logger.log(Level.FINEST, "beforeEncodeEnd interceptors canceled", e);
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
                logger.finest("start afterEncodeEnd of " + rendererInterceptor.getClass().getName());

                rendererInterceptor.afterEncodeEnd(facesContext, uiComponent, this.wrapped);

                logger.finest("afterEncodeEnd of " + rendererInterceptor.getClass().getName() + " finished");
            }
        }
        catch (SkipAfterInterceptorsException e)
        {
            logger.log(Level.FINEST, "afterEncodeEnd interceptors canceled", e);
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
                logger.finest("start beforeGetConvertedValue of " + rendererInterceptor.getClass().getName());

                try
                {
                    rendererInterceptor.beforeGetConvertedValue(facesContext, uiComponent, o, this.wrapped);
                }
                catch (SkipRendererDelegationException e)
                {
                    convertedObject = e.getReturnValueOnException(convertedObject);

                    logger.log(Level.FINEST, "getConvertedValue delegation canceled", e);

                    delegateToWrappedRenderer = false;

                    if(e.isSkipOtherInterceptors())
                    {
                        break;
                    }
                }

                logger.finest("beforeGetConvertedValue of " +
                    rendererInterceptor.getClass().getName() + " finished");
            }
        }
        catch (SkipBeforeInterceptorsException e)
        {
            logger.log(Level.FINEST, "beforeGetConvertedValue interceptors canceled", e);
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
                logger.finest("start afterGetConvertedValue of " + rendererInterceptor.getClass().getName());

                rendererInterceptor.afterGetConvertedValue(facesContext, uiComponent, o, this.wrapped);

                logger.finest("afterGetConvertedValue of " + rendererInterceptor.getClass().getName() + " finished");
            }
        }
        catch (SkipAfterInterceptorsException e)
        {
            logger.log(Level.FINEST, "afterGetConvertedValue interceptors canceled", e);
        }

        return convertedObject;
    }
}
