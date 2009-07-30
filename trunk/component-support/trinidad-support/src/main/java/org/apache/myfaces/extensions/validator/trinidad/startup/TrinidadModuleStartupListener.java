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
import org.apache.myfaces.extensions.validator.core.renderkit.ExtValRendererProxy;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.trinidad.initializer.component.TrinidadComponentInitializer;
import org.apache.myfaces.extensions.validator.trinidad.WebXmlParameter;
import org.apache.myfaces.extensions.validator.trinidad.validation.message.TrinidadFacesMessageFactory;
import org.apache.myfaces.extensions.validator.trinidad.renderkit.ExtValTrinidadRendererProxy;
import org.apache.myfaces.extensions.validator.trinidad.interceptor.TrinidadValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.trinidad.interceptor.TrinidadRendererInterceptor;
import org.apache.myfaces.extensions.validator.trinidad.interceptor.TrinidadMetaDataExtractionInterceptor;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * alternative approach for ExtValRenderKitFactory
 * 
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class TrinidadModuleStartupListener extends AbstractStartupListener
{
    private static final long serialVersionUID = -8034089244903966999L;

    protected void init()
    {
        initTrinidadSupport();
    }

    private void initTrinidadSupport()
    {
        //deactivate default approach
        ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.RENDERKIT_WRAPPER_FACTORY, AbstractRenderKitWrapperFactory.class).deactivate();
        
        String deactivateClientSideValidation = WebXmlParameter.DEACTIVATE_CLIENT_SIDE_TRINIDAD_VALIDATION;

        if(deactivateClientSideValidation == null || !deactivateClientSideValidation.equalsIgnoreCase("true"))
        {
            ExtValContext.getContext().addComponentInitializer(new TrinidadComponentInitializer());
        }

        String deactivateInitCoreOutputLabel = WebXmlParameter.DEACTIVATE_TRINIDAD_CORE_OUTPUT_LABEL_INITIALIZATION;

        if(deactivateInitCoreOutputLabel == null || !deactivateInitCoreOutputLabel.equalsIgnoreCase("true"))
        {
            ExtValContext.getContext().registerRendererInterceptor(new TrinidadRendererInterceptor());
        }

        String deactivateTrinidadValidationExceptionInterceptor =
                WebXmlParameter.DEACTIVATE_TRINIDAD_VALIDATION_EXCEPTION_INTERCEPTOR;

        if(deactivateTrinidadValidationExceptionInterceptor == null ||
                !deactivateTrinidadValidationExceptionInterceptor.equalsIgnoreCase("true"))
        {
            ExtValContext.getContext().addValidationExceptionInterceptor(new TrinidadValidationExceptionInterceptor());
        }

        //deactivate extval renderer proxy - due to an incompatibility with the table renderer
        ExtValContext.getContext()
                .addGlobalProperty(ExtValRendererProxy.KEY, ExtValTrinidadRendererProxy.class.getName());

        ExtValContext.getContext()
                .addGlobalProperty(
                        FactoryNames.FACES_MESSAGE_FACTORY.name(),
                        TrinidadFacesMessageFactory.class.getName());
        /*
         * if there are further incompatible renderers use the following quick-fix:
         *         ExtValContext.getContext()
                .addGlobalProperty(ExtValRendererProxy.KEY, null);
           attention: it causes direct delegation without a check of double invocations
         */

        ExtValContext.getContext().addMetaDataExtractionInterceptor(new TrinidadMetaDataExtractionInterceptor());
    }
}
