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
package org.apache.myfaces.extensions.validator.trinidad.startup;

import org.apache.myfaces.extensions.validator.core.startup.AbstractStartupListener;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.renderkit.AbstractRenderKitWrapperFactory;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.trinidad.initializer.component.TrinidadComponentInitializer;
import org.apache.myfaces.extensions.validator.trinidad.WebXmlParameter;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;

/**
 * alternative approach for ExtValRenderKitFactory
 * 
 * @author Gerhard Petracek
 */
public class TrinidadModuleStartupListener extends AbstractStartupListener
{
    @ToDo(value = Priority.MEDIUM, description = "web.xml parameter to deactivate it")
    protected void init()
    {
        initTrinidadSupport();
    }

    private void initTrinidadSupport()
    {
        //deactivate default approach
        ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.RENDERKIT_WRAPPER_FACTORY, AbstractRenderKitWrapperFactory.class)
            .deactivate();
        
        String deactivateClientSideValidation = WebXmlParameter.DEACTIVATE_CLIENT_SIDE_TRINIDAD_VALIDATION;

        if(deactivateClientSideValidation == null || !deactivateClientSideValidation.equalsIgnoreCase("true"))
        {
            ExtValContext.getContext().addComponentInitializer(new TrinidadComponentInitializer());
        }
    }
}
