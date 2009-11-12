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
import org.apache.myfaces.extensions.validator.util.LogUtils;

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
    protected Renderer wrapped;
    protected ExtValContext extValContext = ExtValContext.getContext();

    public ExtValRendererWrapper(Renderer renderer)
    {
        this.wrapped = new ExtValRendererProxy(renderer);

        LogUtils.trace("extval renderer wrapper created for " + renderer.getClass().getName(), getClass());
    }

    @Override
    public final void decode(FacesContext facesContext, UIComponent uiComponent)
    {
        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            LogUtils.trace("start beforeDecode of " + rendererInterceptor.getClass().getName(),
                getClass());

            rendererInterceptor.beforeDecode(facesContext, uiComponent, this.wrapped);

            LogUtils.trace("beforeDecode of " + rendererInterceptor.getClass().getName() + " finished",
                getClass());
        }

        wrapped.decode(facesContext, uiComponent);

        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            LogUtils.trace("start afterDecode of " + rendererInterceptor.getClass().getName(),
                getClass());

            rendererInterceptor.afterDecode(facesContext, uiComponent, this.wrapped);

            LogUtils.trace("afterDecode of " + rendererInterceptor.getClass().getName() + " finished",
                getClass());
        }
    }

    @Override
    public final void encodeBegin(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            LogUtils.trace("start beforeEncodeBegin of " + rendererInterceptor.getClass().getName(),
                getClass());

            rendererInterceptor.beforeEncodeBegin(facesContext, uiComponent, this.wrapped);

            LogUtils.trace("beforeEncodeBegin of " + rendererInterceptor.getClass().getName() + " finished",
                getClass());
        }

        wrapped.encodeBegin(facesContext, uiComponent);

        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            LogUtils.trace("start afterEncodeBegin of " + rendererInterceptor.getClass().getName(),
                getClass());

            rendererInterceptor.afterEncodeBegin(facesContext, uiComponent, this.wrapped);

            LogUtils.trace("afterEncodeBegin of " + rendererInterceptor.getClass().getName() + " finished",
                getClass());
        }
    }

    @Override
    public final void encodeChildren(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            LogUtils.trace("start beforeEncodeChildren of " + rendererInterceptor.getClass().getName(),
                getClass());

            rendererInterceptor.beforeEncodeChildren(facesContext, uiComponent, this.wrapped);

            LogUtils.trace("beforeEncodeChildren of " + rendererInterceptor.getClass().getName() + " finished",
                getClass());
        }

        wrapped.encodeChildren(facesContext, uiComponent);

        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            LogUtils.trace("start afterEncodeChildren of " + rendererInterceptor.getClass().getName(),
                getClass());

            rendererInterceptor.afterEncodeChildren(facesContext, uiComponent, this.wrapped);

            LogUtils.trace("afterEncodeChildren of " + rendererInterceptor.getClass().getName() + " finished",
                getClass());
        }
    }

    @Override
    public final void encodeEnd(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            LogUtils.trace("start beforeEncodeEnd of " + rendererInterceptor.getClass().getName(),
                getClass());

            rendererInterceptor.beforeEncodeEnd(facesContext, uiComponent, this.wrapped);

            LogUtils.trace("beforeEncodeEnd of " + rendererInterceptor.getClass().getName() + " finished",
                getClass());
        }

        wrapped.encodeEnd(facesContext, uiComponent);

        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            LogUtils.trace("start afterEncodeEnd of " + rendererInterceptor.getClass().getName(),
                getClass());

            rendererInterceptor.afterEncodeEnd(facesContext, uiComponent, this.wrapped);

            LogUtils.trace("afterEncodeEnd of " + rendererInterceptor.getClass().getName() + " finished",
                getClass());
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
        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            LogUtils.trace("start beforeGetConvertedValue of " + rendererInterceptor.getClass().getName(),
                getClass());

            rendererInterceptor.beforeGetConvertedValue(facesContext, uiComponent, o, this.wrapped);

            LogUtils.trace("beforeGetConvertedValue of " + rendererInterceptor.getClass().getName() + " finished",
                getClass());
        }

        Object convertedObject = wrapped.getConvertedValue(facesContext, uiComponent, o);

        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            LogUtils.trace("start afterGetConvertedValue of " + rendererInterceptor.getClass().getName(),
                getClass());

            rendererInterceptor.afterGetConvertedValue(facesContext, uiComponent, o, this.wrapped);

            LogUtils.trace("afterGetConvertedValue of " + rendererInterceptor.getClass().getName() + " finished",
                getClass());
        }

        return convertedObject;
    }
}
