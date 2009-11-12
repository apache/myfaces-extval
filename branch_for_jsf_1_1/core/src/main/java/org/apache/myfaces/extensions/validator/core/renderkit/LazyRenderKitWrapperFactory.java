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
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;

import javax.faces.render.RenderKit;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@ToDo(value = Priority.MEDIUM, description = "test this workaround with mojarra")
@UsageInformation(UsageCategory.INTERNAL)
public class LazyRenderKitWrapperFactory extends DefaultRenderKitWrapperFactory
{
    private AbstractRenderKitWrapperFactory wrapped;

    @Override
    protected RenderKit createWrapper(RenderKit renderKit)
    {
        if(ExtValUtils.isApplicationInitialized())
        {
            tryToInitWrappedRenderKitWrapperFractory();
        }

        return tryToCreateWrapperWithWrappedFactory(renderKit);
    }

    private RenderKit tryToCreateWrapperWithWrappedFactory(RenderKit renderKit)
    {
        if(this.wrapped != null)
        {
            if(this.wrapped.isDeactivated())
            {
                return renderKit;
            }
            return this.wrapped.createWrapper(renderKit);
        }

        return super.createWrapper(renderKit);
    }

    private void tryToInitWrappedRenderKitWrapperFractory()
    {
        if(this.wrapped == null)
        {
            this.wrapped = ExtValContext.getContext().getFactoryFinder()
                .getFactory(FactoryNames.RENDERKIT_WRAPPER_FACTORY, AbstractRenderKitWrapperFactory.class);
        }
    }
}
