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
import org.apache.myfaces.extensions.validator.core.initializer.rendering.RenderingContextInitializer;
import org.apache.myfaces.extensions.validator.core.initializer.rendering.DefaultRenderingContextInitializer;
import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.core.initializer.component.DefaultComponentInitializer;
import org.apache.myfaces.extensions.validator.util.ClassUtils;

import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public class ExtValContext
{
    private static ExtValContext extValContext = new ExtValContext();

    private Map<String, RendererInterceptor> rendererInterceptors = new HashMap<String, RendererInterceptor>();
    private List<String> deniedInterceptors = new ArrayList<String>();

    public static ExtValContext getContext()
    {
        return extValContext;
    }

    public List<RendererInterceptor> getRendererInterceptors()
    {
        return new ArrayList<RendererInterceptor>(rendererInterceptors.values());
    }

    public boolean registerRendererInterceptors(RendererInterceptor rendererInterceptor)
    {
        synchronized (ExtValContext.class)
        {
            if(deniedInterceptors.contains(rendererInterceptor.getInterceptorId()))
            {
                return false;
            }

            rendererInterceptors.put(rendererInterceptor.getInterceptorId(), rendererInterceptor);
        }
        return true;
    }

    public void deregisterRendererInterceptor(Class rendererInterceptorClass)
    {
        RendererInterceptor rendererInterceptor =
            (RendererInterceptor) ClassUtils.tryToInstantiateClass(rendererInterceptorClass);

        synchronized (ExtValContext.class)
        {
            rendererInterceptors.remove(rendererInterceptor.getInterceptorId());
        }
    }

    //if an interceptor hasn't been registered so far, it should be denied at future registrations
    public void denyRendererInterceptor(Class rendererInterceptorClass)
    {
        RendererInterceptor rendererInterceptor =
            (RendererInterceptor) ClassUtils.tryToInstantiateClass(rendererInterceptorClass);

        synchronized (ExtValContext.class)
        {
            deniedInterceptors.add(rendererInterceptor.getInterceptorId());
        }
        deregisterRendererInterceptor(rendererInterceptorClass);
    }

    public static void addRenderingContextInitializer(RenderingContextInitializer renderingContextInitializer)
    {
        DefaultRenderingContextInitializer.addRenderingContextInitializer(renderingContextInitializer);
    }

    public void addComponentInitializer(ComponentInitializer componentInitializer)
    {
        DefaultComponentInitializer.addComponentInitializer(componentInitializer);
    }
}
