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

import org.apache.myfaces.extensions.validator.core.renderkit.AbstractRenderKitWrapperFactory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.render.RenderKit;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class GenericRenderKitWrapperFactory extends AbstractRenderKitWrapperFactory
{
    private Map<Class<? extends RenderKit>, RenderKit> renderKitCache =
            new ConcurrentHashMap<Class<? extends RenderKit>, RenderKit>();

    protected RenderKit createWrapper(RenderKit renderKit)
    {
        logger.finest("extval renderkit wrapper created for " + renderKit.getClass().getName() + " via cglib");

        if(!this.renderKitCache.containsKey(renderKit.getClass()))
        {
            RenderKit wrappedRenderKit = ExtValGenericRenderKit.newInstance(renderKit);
            this.renderKitCache.put(renderKit.getClass(), wrappedRenderKit);
        }

        return this.renderKitCache.get(renderKit.getClass());
    }
}
