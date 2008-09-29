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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.ConverterException;
import javax.faces.render.Renderer;
import java.io.IOException;

/**
 * Default approach to avoid proxies for converters and the adapter fallback.
 * It requires that components delegate getConvertedValue to a renderer.
 * If it isn't the case for your component lib use:
 * org.apache.myfaces.extensions.validator.core.proxy.ExtValApplicationFactory
 * and
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

    public ExtValRendererWrapper(Renderer wrapped)
    {
        this.wrapped = new ExtValRendererProxy(wrapped);
    }

    @Override
    public final void decode(FacesContext facesContext, UIComponent uiComponent)
    {
        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            rendererInterceptor.beforeDecode(facesContext, uiComponent, this.wrapped);
        }

        wrapped.decode(facesContext, uiComponent);

        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            rendererInterceptor.afterDecode(facesContext, uiComponent, this.wrapped);
        }
    }

    @Override
    public final void encodeBegin(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            rendererInterceptor.beforeEncodeBegin(facesContext, uiComponent, this.wrapped);
        }

        wrapped.encodeBegin(facesContext, uiComponent);

        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            rendererInterceptor.afterEncodeBegin(facesContext, uiComponent, this.wrapped);
        }
    }

    @Override
    public final void encodeChildren(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            rendererInterceptor.beforeEncodeChildren(facesContext, uiComponent, this.wrapped);
        }

        wrapped.encodeChildren(facesContext, uiComponent);

        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            rendererInterceptor.afterEncodeChildren(facesContext, uiComponent, this.wrapped);
        }
    }

    @Override
    public final void encodeEnd(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            rendererInterceptor.beforeEncodeEnd(facesContext, uiComponent, this.wrapped);
        }

        wrapped.encodeEnd(facesContext, uiComponent);

        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            rendererInterceptor.afterEncodeEnd(facesContext, uiComponent, this.wrapped);
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

    public final Object getConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o)
        throws ConverterException
    {
        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            rendererInterceptor.beforeGetConvertedValue(facesContext, uiComponent, o, this.wrapped);
        }

        Object convertedObject = wrapped.getConvertedValue(facesContext, uiComponent, o);

        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            rendererInterceptor.afterGetConvertedValue(facesContext, uiComponent, o, this.wrapped);
        }

        return convertedObject;
    }
}
