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
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.storage.StorageManagerHolder;
import org.apache.myfaces.extensions.validator.core.renderkit.AbstractRenderKitWrapperFactory;
import org.apache.myfaces.extensions.validator.core.renderkit.ExtValRendererProxy;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.trinidad.initializer.component.TrinidadComponentInitializer;
import org.apache.myfaces.extensions.validator.trinidad.ExtValTrinidadSupportModuleConfiguration;
import org.apache.myfaces.extensions.validator.trinidad.DefaultExtValTrinidadSupportModuleConfiguration;
import org.apache.myfaces.extensions.validator.trinidad.storage.TrinidadClientValidatorStorage;
import org.apache.myfaces.extensions.validator.trinidad.storage.DefaultClientValidatorStorageManager;
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
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class TrinidadModuleStartupListener extends AbstractStartupListener
{
    private static final long serialVersionUID = -8034089244903966999L;

    protected void initModuleConfig()
    {
        ExtValTrinidadSupportModuleConfiguration.use(new DefaultExtValTrinidadSupportModuleConfiguration(), false);
    }
    
    protected void init()
    {
        initModuleConfig();

        deactivateDefaultExtValRenderKitWrapperFactory();

        initClientSideValidationSupport();

        initLabelInitializationSupport();

        initValidationExceptionInterception();

        //replaceDefaultProxyWithTrinidadRendererProxy();

        initTrinidadFacesMessageFactory();
        /*
         * if there are further incompatible renderers use the following quick-fix:
         *         ExtValContext.getContext()
                .addGlobalProperty(ExtValRendererProxy.KEY, null);
           attention: it causes direct delegation without a check of double invocations
         */

        initTrinidadMetaDataExtractionInterceptor();

        initTrinidadClientValidatorStorage();

        initRequiredInitialization();
    }

    private void deactivateDefaultExtValRenderKitWrapperFactory()
    {
        ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.RENDERKIT_WRAPPER_FACTORY, AbstractRenderKitWrapperFactory.class).deactivate();
    }

    private void initClientSideValidationSupport()
    {
        if(!ExtValTrinidadSupportModuleConfiguration.get().deactivateClientSideValidation())
        {
            ExtValContext.getContext().addComponentInitializer(new TrinidadComponentInitializer());
        }
    }

    private void initLabelInitializationSupport()
    {
        if(!ExtValTrinidadSupportModuleConfiguration.get().deactivateCoreOutputLabelInitialization())
        {
            ExtValContext.getContext().registerRendererInterceptor(new TrinidadRendererInterceptor());
        }
    }

    private void initValidationExceptionInterception()
    {
        if(!ExtValTrinidadSupportModuleConfiguration.get().deactivateValidationExceptionInterceptor())
        {
            ExtValContext.getContext().addValidationExceptionInterceptor(new TrinidadValidationExceptionInterceptor());
        }
    }

    private void replaceDefaultProxyWithTrinidadRendererProxy()
    {
        ExtValContext.getContext()
                .addGlobalProperty(ExtValRendererProxy.KEY, ExtValTrinidadRendererProxy.class.getName());
    }

    private void initTrinidadFacesMessageFactory()
    {
        ExtValContext.getContext()
                .addGlobalProperty(
                        FactoryNames.FACES_MESSAGE_FACTORY.name(),
                        TrinidadFacesMessageFactory.class.getName());
    }

    private void initTrinidadMetaDataExtractionInterceptor()
    {
        ExtValContext.getContext().addMetaDataExtractionInterceptor(new TrinidadMetaDataExtractionInterceptor());
    }

    private void initTrinidadClientValidatorStorage()
    {
        ExtValContext.getContext().getFactoryFinder()
                .getFactory(FactoryNames.STORAGE_MANAGER_FACTORY, StorageManagerHolder.class)
                .setStorageManager(TrinidadClientValidatorStorage.class,
                        new DefaultClientValidatorStorageManager(), false);
    }

    protected void initRequiredInitialization()
    {
        DefaultExtValCoreConfiguration.overruleActivateRequiredInitialization(Boolean.TRUE, true);

        //there is no support for client-side severity aware validation -> don't reset the value
        DefaultExtValCoreConfiguration.overruleDeactivateRequiredAttributeSupport(Boolean.FALSE, false);
    }

    private boolean isRequiredInitializationDeactivated()
    {
        return !ExtValCoreConfiguration.get().activateRequiredInitialization();
    }
}
