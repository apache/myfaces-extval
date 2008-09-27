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
package org.apache.myfaces.extensions.validator.generic;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.RendererInterceptor;
import org.apache.myfaces.extensions.validator.core.RendererProxy;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.ConverterException;
import javax.faces.render.Renderer;
import java.io.IOException;
import java.lang.reflect.Method;

import net.sf.cglib.proxy.MethodInterceptor;
import net.sf.cglib.proxy.Enhancer;
import net.sf.cglib.proxy.MethodProxy;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValGenericRendererWrapper extends Renderer implements MethodInterceptor
{
    protected Renderer wrapped;
    protected ExtValContext extValContext = ExtValContext.getContext();

    public static Renderer newInstance(Renderer renderer)
    {
        Class currentClass = renderer.getClass();

        //to avoid re-wrapping - occurs e.g. under solaris + bea weblogic
        if (currentClass.getName().contains("$$EnhancerByCGLIB$$") ||
            currentClass.getName().contains("$$FastClassByCGLIB$$"))
        {
            return renderer;
        }

        Enhancer enhancer = new Enhancer();
        enhancer.setSuperclass(renderer.getClass());
        enhancer.setCallback(new ExtValGenericRendererWrapper(renderer));

        return (Renderer) enhancer.create();
    }

    public Object intercept(Object obj, Method method, Object[] args, MethodProxy proxy) throws Throwable
    {
        if (method.getName().equals("getConvertedValue"))
        {
            return getConvertedValue((FacesContext)args[0], (UIComponent)args[1], args[2]);
        }
        else if (method.getName().equals("decode"))
        {
            decode((FacesContext)args[0], (UIComponent)args[1]);
        }
        else if (method.getName().equals("encodeBegin"))
        {
            encodeBegin((FacesContext)args[0], (UIComponent)args[1]);
        }
        else if (method.getName().equals("encodeChildren"))
        {
            encodeChildren((FacesContext)args[0], (UIComponent)args[1]);
        }
        else if (method.getName().equals("encodeEnd"))
        {
            encodeEnd((FacesContext)args[0], (UIComponent)args[1]);
        }
        else if (method.getName().equals("convertClientId"))
        {
            return convertClientId((FacesContext)args[0], (String)args[1]);
        }
        else if (method.getName().equals("getRendersChildren"))
        {
            return getRendersChildren();
        }
        else
        {
            return proxy.invokeSuper(obj, args);
        }
        return null;
    }

    public ExtValGenericRendererWrapper(Renderer wrapped)
    {
        this.wrapped = new RendererProxy(wrapped);
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
        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            rendererInterceptor.beforeConvertClientId(facesContext, s, this.wrapped);
        }

        String clientId = wrapped.convertClientId(facesContext, s);

        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            rendererInterceptor.afterConvertClientId(facesContext, s, this.wrapped);
        }

        return clientId;
    }

    @Override
    public final boolean getRendersChildren()
    {
        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            rendererInterceptor.beforeGetRendersChildren(this.wrapped);
        }

        boolean rendersChildren = wrapped.getRendersChildren();

        for(RendererInterceptor rendererInterceptor : extValContext.getRendererInterceptors())
        {
            rendererInterceptor.afterGetRendersChildren(this.wrapped);
        }

        return rendersChildren;
    }

    public Object getConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o)
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