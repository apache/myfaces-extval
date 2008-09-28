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
import org.apache.myfaces.extensions.validator.core.ClassMappingFactory;

import javax.faces.render.RenderKit;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public abstract class AbstractRenderKitWrapperFactory implements ClassMappingFactory<RenderKit, RenderKit>
{
    protected AbstractRenderKitWrapperFactory wrapped;

    public void addRenderKitWrapperFactory(AbstractRenderKitWrapperFactory renderKitWrapperFactory)
    {
        if(this.wrapped != null)
        {
            this.wrapped.addRenderKitWrapperFactory(renderKitWrapperFactory);
            return;
        }

        this.wrapped = renderKitWrapperFactory;
    }

    public final RenderKit create(RenderKit renderKit)
    {
        RenderKit result = null;

        if(this.wrapped != null)
        {
            result = this.wrapped.create(renderKit);
        }

        if(result == null)
        {
            return createWrapper(renderKit);
        }

        return result;
    }

    protected abstract RenderKit createWrapper(RenderKit renderKit);
}