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

import org.apache.myfaces.extensions.validator.core.proxy.ProxyHelper;
import org.apache.myfaces.extensions.validator.core.renderkit.ExtValRendererProxy;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameter;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.lang.annotation.Annotation;
import java.util.logging.Logger;

/**
 * 'custom' as prefix is used for 'optional' configurations. that means
 * if a method returns null extval uses a different approach to find an implementation e.g. via a naming convention
 * -> all other methods aren't allowed to return null if there is no additional rule
 *
 * @author Gerhard Petracek
 * @since r4
 */
@UsageInformation(UsageCategory.INTERNAL)
public abstract class ExtValCoreConfiguration implements ExtValModuleConfiguration
{
    private static ExtValContext extValContext = null;

    private static final Logger LOGGER = Logger.getLogger(ExtValCoreConfiguration.class.getName());

    private static final String MISSING_MODULE_CONFIG_MESSAGE =
            "no config for " + ExtValCoreConfiguration.class.getName() + " found. " +
            "maybe the call of ExtValCoreConfig#get is triggered before the registration process." +
            "the default config gets used.";

    protected ExtValCoreConfiguration()
    {
    }

    /**
     * Don't access ExtValContext during initialization of the class.  OpenWebBeans initializes all classes during
     * startup of the WebContainer.  extValContext constructor tries to access Web.xml parameters through FacesContext
     * which isn't available yet.
     * @return The ExtValContext
     */
    private static ExtValContext getExtValContext()
    {
        if (extValContext == null)
        {
            extValContext = ExtValContext.getContext();
        }
        return extValContext;
    }
    
    public static ExtValCoreConfiguration get()
    {
        ExtValCoreConfiguration moduleConfig = getExtValContext().getModuleConfiguration(ExtValCoreConfiguration.class);

        if(moduleConfig == null)
        {
            LOGGER.fine(MISSING_MODULE_CONFIG_MESSAGE);
        }
        return moduleConfig != null ? moduleConfig : new DefaultExtValCoreConfiguration();
    }

    @UsageInformation(UsageCategory.INTERNAL)
    public static boolean use(ExtValCoreConfiguration config, boolean forceOverride)
    {
        return getExtValContext().addModuleConfiguration(ExtValCoreConfiguration.class, config, forceOverride);
    }

    /*
     * web.xml config
     */
    public abstract String customMessageBundleBaseName();

    public abstract String customBasePackage();

    public abstract String customInformationProviderBeanClassName();

    public abstract String customComponentMetaDataExtractorClassName();

    public abstract String customValidationParameterExtractorClassName();

    public abstract String customStaticValidationStrategyMappingSource();

    public abstract String customComponentInitializerClassName();

    public abstract String customValidationExceptionInterceptorClassName();

    public abstract String customPropertyValidationInterceptorClassName();

    public abstract String customMetaDataExtractionInterceptorClassName();

    /*
     * others
     */
    public abstract ProxyHelper proxyHelper();

    public abstract ProjectStageResolver projectStageResolver();

    public abstract Class<? extends ExtValRendererProxy> rendererProxy();

    /*
     * ConstraintSource
     */

    public abstract Class<? extends Annotation> constraintSourceAnnotation();

    public abstract Class<? extends Annotation> ignoreConstraintSourceAnnotation();

    public abstract Class<? extends Annotation> targetPropertyAnnotation();

    public abstract Class<? extends Annotation> targetPropertyIdAnnotation();

    /*
     * validation parameter
     */

    public abstract Class violationSeverity();

    public abstract Class<? extends ValidationParameter> disableClientSideValidationValidationParameter();

    /*
     * name mapper
     */
    public abstract String customValidationStrategyToMessageResolverNameMapperClassName();

    public abstract String customMetaDataToValidationStrategyNameMapperClassName();

    public abstract String customValidationStrategyToMetaDataTransformerNameMapperClassName();

    /*
     * filter
     */
    public abstract String customMetaDataStorageFilterClassName();

    /*
     * factories
     */
    public abstract String customValidationStrategyFactoryClassName();

    public abstract String customMessageResolverFactoryClassName();

    public abstract String customComponentMetaDataExtractorFactoryClassName();

    public abstract String customValidationParameterExtractorFactoryClassName();

    public abstract String customValidationParameterFactoryClassName();

    public abstract String customMetaDataTransformerFactoryClassName();

    public abstract String customStorageManagerFactoryClassName();

    public abstract String customFacesMessageFactoryClassName();

    /*
     * activate
     */
    public abstract boolean activateRequiredInitialization();

    /*
     * deactivate
     */
    //currently just used by AbstractValidationErrorMessageResolver
    public abstract boolean deactivateDefaultConvention();

    public abstract boolean deactivateDefaultNameMappers();

    public abstract boolean deactivateElResolver();

    public abstract boolean deactivateComponentInitialization();

    public abstract boolean deactivateValidationParameters();

    public abstract boolean deactivateRenderKitFactory();

    //there is nothing like DEACTIVATE_DEFAULT_VALIDATION_INTERCEPTOR
    //use ExtValContext.getContext().denyRendererInterceptor(...) within an extval-StartupListener

    public abstract boolean deactivateRequiredAttributeSupport();

    /*
     * spec parameters
     */
    public abstract boolean interpretEmptyStringSubmittedValuesAsNull();

    public abstract boolean validateEmptyFields();
}
