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
package org.apache.myfaces.extensions.validator.core.initializer.rendering;

import org.apache.myfaces.extensions.validator.core.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.render.RenderKit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation({UsageCategory.INTERNAL, UsageCategory.CUSTOMIZABLE})
public class DefaultRenderingContextInitializerFactory implements
    ClassMappingFactory<RenderKit, RenderingContextInitializer>
{
    private static Map<String, RenderingContextInitializer> renderKitToRenderingContextInitializerMapping
                                                        = new HashMap<String, RenderingContextInitializer>();
    private static List<String> renderingContextClassNames = new ArrayList<String>();

    static
    {
        renderingContextClassNames
            .add(WebXmlParameter.CUSTOM_RENDERING_CONTEXT_INITIALIZER);
        renderingContextClassNames
            .add(ExtValUtils.getInformationProviderBean().getCustomRenderingContextInitializer());
        renderingContextClassNames
            .add(TrinidadRenderingContextInitializer.class.getName());
    }

    public RenderingContextInitializer create(RenderKit renderKit)
    {
        String renderKitKey = renderKit.getClass().getName();

        if(renderKitToRenderingContextInitializerMapping.containsKey(renderKitKey))
        {
            return renderKitToRenderingContextInitializerMapping.get(renderKitKey);
        }

        RenderingContextInitializer renderingContextInitializer;

        for (String renderingContextInitializerName : renderingContextClassNames)
        {
            renderingContextInitializer =
                (RenderingContextInitializer)ClassUtils.tryToInstantiateClassForName(renderingContextInitializerName);

            if(renderingContextInitializer != null)
            {
                renderKitToRenderingContextInitializerMapping.put(renderKitKey, renderingContextInitializer );
                return renderingContextInitializer;
            }
        }

        return null;
    }
}
