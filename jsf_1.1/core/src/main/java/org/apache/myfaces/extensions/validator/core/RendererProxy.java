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
package org.apache.myfaces.extensions.validator.core;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;
import javax.faces.component.UIComponent;
import javax.faces.convert.ConverterException;
import java.io.IOException;

/**
 * to avoid multiple class within renderer interceptors (e.g. for encode, decode,...
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
class RendererProxy extends Renderer
{
    private Renderer wrapped;

    private boolean isDecodeCalled = false;
    private boolean isEncodeBeginCalled = false;
    private boolean isEncodeChildrenCalled = false;
    private boolean isEncodeEndCalled = false;

    private Object convertedValue = null;
    private String clientId = null;
    private Boolean rendersChildren;

    public RendererProxy(Renderer renderer)
    {
        this.wrapped = renderer;
    }

    public void decode(FacesContext facesContext, UIComponent uiComponent)
    {
        if(!isDecodeCalled)
        {
            wrapped.decode(facesContext, uiComponent);
        }
    }

    public void encodeBegin(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        if(!isEncodeBeginCalled)
        {
            wrapped.encodeBegin(facesContext, uiComponent);
        }
    }

    public void encodeChildren(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        if(!isEncodeChildrenCalled)
        {
            wrapped.encodeChildren(facesContext, uiComponent);
        }
    }

    public void encodeEnd(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        if(!isEncodeEndCalled)
        {
            wrapped.encodeEnd(facesContext, uiComponent);
        }
    }

    public String convertClientId(FacesContext facesContext, String s)
    {
        if(this.clientId == null)
        {
            this.clientId = wrapped.convertClientId(facesContext, s);
        }
        return this.clientId;
    }

    public boolean getRendersChildren()
    {
        if(rendersChildren == null)
        {
            this.rendersChildren = wrapped.getRendersChildren();
        }
        return rendersChildren;
    }

    public Object getConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o)
        throws ConverterException
    {
        if(this.convertedValue == null)
        {
            this.convertedValue = wrapped.getConvertedValue(facesContext, uiComponent, o);
        }
        return this.convertedValue;
    }
}
