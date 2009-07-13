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
package org.apache.myfaces.extensions.validator;

import org.apache.myfaces.extensions.validator.baseval.WebXmlParameter;
import org.apache.myfaces.extensions.validator.baseval.annotation.SkipValidationSupport;
import org.apache.myfaces.extensions.validator.core.startup.AbstractStartupListener;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.factory.AbstractNameMapperAwareFactory;
import org.apache.myfaces.extensions.validator.core.storage.StorageManagerHolder;
import org.apache.myfaces.extensions.validator.core.storage.StorageManager;
import org.apache.myfaces.extensions.validator.core.storage.GroupStorage;
import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;
import org.apache.myfaces.extensions.validator.core.interceptor.ValidationInterceptor;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticResourceBundleConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationNames;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticInMemoryConfiguration;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.crossval.recorder.CrossValidationUserInputRecorder;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorage;
import org.apache.myfaces.extensions.validator.crossval.storage.DefaultCrossValidationStorageManager;
import org.apache.myfaces.extensions.validator.crossval.storage.mapper.CrossValidationStorageNameMapper;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class PropertyValidationModuleStartupListener extends AbstractStartupListener
{
    private static final long serialVersionUID = -2474361612857222283L;

    protected void init()
    {
        ExtValContext.getContext().addProcessedInformationRecorder(new CrossValidationUserInputRecorder());

        initStaticStrategyMappings();
        initDefaultComponentInitializer();
        initDefaultValidationExceptionInterceptor();
        addSkipValidationSupport();
        initStorageManagerAndNameMappers();
    }

    private void initStaticStrategyMappings()
    {
        String jpaBasedValidation = WebXmlParameter.DEACTIVATE_JPA_BASED_VALIDATION;
        if (jpaBasedValidation == null
                || !jpaBasedValidation.equalsIgnoreCase("true"))
        {
            StaticConfiguration<String, String> staticConfig = new StaticResourceBundleConfiguration();
            staticConfig.setSourceOfMapping(
                ExtValInformation.EXTENSIONS_VALIDATOR_BASE_PACKAGE_NAME +".jpa_strategy_mappings");

            ExtValContext.getContext().addStaticConfiguration(
             StaticConfigurationNames.META_DATA_TO_VALIDATION_STRATEGY_CONFIG, staticConfig);
        }
    }

    @ToDo(value = Priority.MEDIUM, description = "web.xml parameter to deactivate it")
    private void initDefaultComponentInitializer()
    {
        ExtValContext.getContext().addComponentInitializer(new HtmlCoreComponentsComponentInitializer());
    }

    @ToDo(value = Priority.MEDIUM, description = "web.xml parameter to deactivate it")
    private void initDefaultValidationExceptionInterceptor()
    {
        ExtValContext.getContext().addValidationExceptionInterceptor(
                new HtmlCoreComponentsValidationExceptionInterceptor());
    }

    private void addSkipValidationSupport()
    {
        if(logger.isInfoEnabled())
        {
            logger.info("adding support for @SkipValidation");
        }

        ExtValContext.getContext().denyRendererInterceptor(ValidationInterceptor.class);
        ExtValContext.getContext().registerRendererInterceptor(new ValidationInterceptorWithSkipValidationSupport());

        StaticInMemoryConfiguration config = new StaticInMemoryConfiguration();
        //it's just required to set the target
        config.addMapping(CommonMetaDataKeys.SKIP_VALIDATION, SkipValidationSupport.class.getName());

        ExtValContext.getContext()
                .addStaticConfiguration(StaticConfigurationNames.SKIP_VALIDATION_SUPPORT_CONFIG, config);

        //config.addMapping(CommonMetaDataKeys.SKIP_VALIDATION, RequiredStrategy.class.getName());
    }

    @SuppressWarnings({"unchecked"})
    private void initStorageManagerAndNameMappers()
    {
        StorageManagerHolder storageManagerHolder =
                (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.STORAGE_MANAGER_FACTORY, StorageManagerHolder.class));

        //cross-validation
        DefaultCrossValidationStorageManager crossValidationStorageManager = new DefaultCrossValidationStorageManager();
        crossValidationStorageManager.register(new CrossValidationStorageNameMapper());
        storageManagerHolder.setStorageManager(CrossValidationStorage.class, crossValidationStorageManager, false);

        //group-validation light
        StorageManager storageManager = storageManagerHolder.getStorageManager(GroupStorage.class);

        if(storageManager instanceof AbstractNameMapperAwareFactory)
        {
            ((AbstractNameMapperAwareFactory<String>)storageManager)
                    .register(new PropertyValidationGroupStorageNameMapper());
        }
    }
}
