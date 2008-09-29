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

import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;
import javax.faces.component.UIComponent;
import javax.faces.convert.ConverterException;
import java.io.IOException;
import java.util.Map;
import java.util.HashMap;

/**
 * to avoid multiple class within renderer interceptors (e.g. for encode, decode,...
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValRendererProxy extends Renderer
{
    private Renderer wrapped;

    public ExtValRendererProxy(Renderer renderer)
    {
        this.wrapped = renderer;
    }

    public void decode(FacesContext facesContext, UIComponent uiComponent)
    {
        RendererProxyEntry entry = getOrInitEntry(facesContext, uiComponent);

        if (!entry.isDecodeCalled())
        {
            entry.setDecodeCalled(true);
            wrapped.decode(facesContext, uiComponent);
        }
    }

    public void encodeBegin(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        RendererProxyEntry entry = getOrInitEntry(facesContext, uiComponent);

        if (!entry.isEncodeBeginCalled())
        {
            entry.setEncodeBeginCalled(true);
            wrapped.encodeBegin(facesContext, uiComponent);
        }
    }

    public void encodeChildren(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        RendererProxyEntry entry = getOrInitEntry(facesContext, uiComponent);

        if (!entry.isEncodeChildrenCalled())
        {
            entry.setEncodeChildrenCalled(true);
            wrapped.encodeChildren(facesContext, uiComponent);
        }
    }

    public void encodeEnd(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        RendererProxyEntry entry = getOrInitEntry(facesContext, uiComponent);

        if (!entry.isEncodeEndCalled())
        {
            entry.setEncodeEndCalled(true);
            wrapped.encodeEnd(facesContext, uiComponent);
        }
    }

    public String convertClientId(FacesContext facesContext, String s)
    {
        return wrapped.convertClientId(facesContext, s);
    }

    public boolean getRendersChildren()
    {
        return wrapped.getRendersChildren();
    }

    public Object getConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o)
        throws ConverterException
    {
        RendererProxyEntry entry = getOrInitEntry(facesContext, uiComponent);

        if (entry.getConvertedValue() == null)
        {
            entry.setConvertedValue(wrapped.getConvertedValue(facesContext, uiComponent, o));
        }
        return entry.getConvertedValue();
    }

    private RendererProxyEntry getOrInitEntry(FacesContext facesContext, UIComponent uiComponent)
    {
        String clientId = uiComponent.getClientId(facesContext);

        if (!getOrInitComponentProxyMapping().containsKey(clientId))
        {
            getOrInitComponentProxyMapping().put(clientId, new RendererProxyEntry());
        }
        return getOrInitComponentProxyMapping().get(clientId);
    }

    private static final String PROXY_STORAGE_NAME = ExtValRendererProxy.class.getName() + ":STORAGE";

    private Map<String, RendererProxyEntry> getOrInitComponentProxyMapping()
    {
        Map requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();

        if(!requestMap.containsKey(PROXY_STORAGE_NAME))
        {
            requestMap.put(PROXY_STORAGE_NAME, new HashMap<String, Map<String, RendererProxyEntry>>());
        }

        Map<String, Map<String, RendererProxyEntry>> proxyStorage =
            ((Map<String, Map<String, RendererProxyEntry>>)requestMap.get(PROXY_STORAGE_NAME));

        String key = this.wrapped.getClass().getName();

        if(!proxyStorage.containsKey(key))
        {
            proxyStorage.put(key, new HashMap<String, RendererProxyEntry>());
        }

        return proxyStorage.get(key);
    }
}
