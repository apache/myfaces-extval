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

import org.apache.myfaces.extensions.validator.core.ExtValRenderKit;
import org.apache.myfaces.extensions.validator.core.ExtValRendererWrapper;

import javax.faces.render.RenderKit;
import javax.faces.render.Renderer;
import javax.faces.render.ResponseStateManager;
import javax.faces.context.ResponseWriter;
import javax.faces.context.ResponseStream;
import java.io.Writer;
import java.io.OutputStream;
import java.lang.reflect.Method;

import net.sf.cglib.proxy.MethodInterceptor;
import net.sf.cglib.proxy.Enhancer;
import net.sf.cglib.proxy.MethodProxy;

/**
 * @author Gerhard Petracek
 */
public class ExtValGenericRenderKit extends RenderKit implements MethodInterceptor
{
    protected ExtValRenderKit extValRenderKit;

    public static final String ID = "EXTVAL_GENERIC_RENDERKIT";

    public static RenderKit newInstance(RenderKit renderKit)
    {
        Class currentClass = renderKit.getClass();

        //it's not possible to wrap the converter again - occurs e.g. under solaris + bea weblogic
        if (currentClass.getName().contains("$$EnhancerByCGLIB$$")
            || currentClass.getName().contains("$$FastClassByCGLIB$$"))
        {
            return renderKit;
        }

        Enhancer enhancer = new Enhancer();
        enhancer.setSuperclass(renderKit.getClass());
        enhancer.setCallback(new ExtValGenericRenderKit(renderKit));

        return (RenderKit) enhancer.create();
    }

    public Object intercept(Object obj, Method method, Object[] args, MethodProxy proxy) throws Throwable
    {
        if (method.getName().equals("addRenderer"))
        {
            if (!((Renderer) args[2] instanceof ExtValRendererWrapper))
            {
                return proxy.invokeSuper(obj, args);
            }
            else
            {
                args[2] = ExtValGenericRendererWrapper.newInstance((Renderer)args[2]);
                return proxy.invokeSuper(obj, args);
            }
        }

        Object result = proxy.invokeSuper(obj, args);

        if(method.getName().equals("getRenderer"))
        {
            if(result instanceof Renderer && !(result instanceof ExtValGenericRendererWrapper))
            {
                return ExtValGenericRendererWrapper.newInstance((Renderer)result);
            }
        }

        return result;
    }

    public ExtValGenericRenderKit(RenderKit wrapped)
    {
        this.extValRenderKit = new ExtValRenderKit(wrapped);
    }

    @Override
    public void addRenderer(String family, String rendererType, Renderer renderer)
    {
        this.extValRenderKit.addRenderer(family, rendererType, renderer);
    }

    @Override
    public Renderer getRenderer(String family, String rendererType)
    {
        return this.extValRenderKit.getRenderer(family, rendererType);
    }

    @Override
    public ResponseStateManager getResponseStateManager()
    {
        return this.extValRenderKit.getResponseStateManager();
    }

    @Override
    public ResponseWriter createResponseWriter(Writer writer, String s, String s1)
    {
        return this.extValRenderKit.createResponseWriter(writer, s, s1);
    }

    @Override
    public ResponseStream createResponseStream(OutputStream outputStream)
    {
        return this.extValRenderKit.createResponseStream(outputStream);
    }
}