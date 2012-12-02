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
import org.apache.myfaces.extensions.validator.util.ClassUtils;

import javax.faces.render.RenderKit;

/**
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultRenderKitWrapperFactory extends AbstractRenderKitWrapperFactory
{
    private RenderKit renderKit;
    private static final String GENERIC_RENDER_KIT_WRAPPER_FACTORY =
            "org.apache.myfaces.extensions.validator.generic.renderkit.GenericRenderKitWrapperFactory";
    private static Boolean useGenericRenderKitWrapperFactory = null;

    protected RenderKit createWrapper(RenderKit renderKit)
    {
        logger.finest("extval renderkit wrapper created for " + renderKit.getClass().getName());

        //workaround for mojarra (EXTVAL-38)
        if(useGenericRenderKitWrapperFactory == null)
        {
            Class genericFactory = ClassUtils.tryToLoadClassForName(GENERIC_RENDER_KIT_WRAPPER_FACTORY);
            useGenericRenderKitWrapperFactory = genericFactory != null;
        }

        if(useGenericRenderKitWrapperFactory)
        {
            AbstractRenderKitWrapperFactory renderKitWrapperFactory = (AbstractRenderKitWrapperFactory)ClassUtils
                    .tryToInstantiateClassForName(GENERIC_RENDER_KIT_WRAPPER_FACTORY);
            return renderKitWrapperFactory.createWrapper(renderKit);
        }

        if(this.renderKit == null)
        {
            this.renderKit = new ExtValRenderKit(renderKit);
        }
        return this.renderKit;
    }
}
