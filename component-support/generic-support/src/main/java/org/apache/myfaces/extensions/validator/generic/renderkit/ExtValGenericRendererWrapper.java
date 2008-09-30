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
package org.apache.myfaces.extensions.validator.generic.renderkit;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.core.renderkit.ExtValRendererWrapper;
import org.apache.myfaces.extensions.validator.util.LogUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;
import java.lang.reflect.Method;

import net.sf.cglib.proxy.MethodInterceptor;
import net.sf.cglib.proxy.Enhancer;
import net.sf.cglib.proxy.MethodProxy;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public final class ExtValGenericRendererWrapper extends ExtValRendererWrapper implements MethodInterceptor
{
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
        if (method.getName().equals("getConvertedValue") && args[1] instanceof UIComponent)
        {
            return getConvertedValue((FacesContext)args[0], (UIComponent)args[1], args[2]);
        }
        else if (method.getName().equals("decode") && args[1] instanceof UIComponent)
        {
            decode((FacesContext)args[0], (UIComponent)args[1]);
        }
        else if (method.getName().equals("encodeBegin") && args[1] instanceof UIComponent)
        {
            encodeBegin((FacesContext)args[0], (UIComponent)args[1]);
        }
        else if (method.getName().equals("encodeChildren") && args[1] instanceof UIComponent)
        {
            encodeChildren((FacesContext)args[0], (UIComponent)args[1]);
        }
        else if (method.getName().equals("encodeEnd") && args[1] instanceof UIComponent)
        {
            encodeEnd((FacesContext)args[0], (UIComponent)args[1]);
        }
        else if (method.getName().equals("convertClientId") && args[1] instanceof String)
        {
            return convertClientId((FacesContext)args[0], (String)args[1]);
        }
        else if (method.getName().equals("getRendersChildren"))
        {
            return getRendersChildren();
        }
        else
        {
            LogUtils.trace("method " + method.getName() + " called without rendering-interceptors", getClass());

            return proxy.invokeSuper(obj, args);
        }
        return null;
    }

    public ExtValGenericRendererWrapper(Renderer wrapped)
    {
        super(wrapped);
    }
}
