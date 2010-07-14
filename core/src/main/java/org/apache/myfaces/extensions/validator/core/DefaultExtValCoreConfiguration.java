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
import org.apache.myfaces.extensions.validator.core.proxy.DefaultProxyHelper;
import org.apache.myfaces.extensions.validator.core.renderkit.ExtValRendererProxy;
import org.apache.myfaces.extensions.validator.core.validation.ConstraintSource;
import org.apache.myfaces.extensions.validator.core.validation.IgnoreConstraintSource;
import org.apache.myfaces.extensions.validator.core.validation.TargetProperty;
import org.apache.myfaces.extensions.validator.core.validation.TargetPropertyId;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DisableClientSideValidation;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameter;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ViolationSeverity;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 * @since r4
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultExtValCoreConfiguration extends ExtValCoreConfiguration
{
    public String customMessageBundleBaseName()
    {
        return WebXmlParameter.CUSTOM_MESSAGE_BUNDLE;
    }

    public String customBasePackage()
    {
        return WebXmlParameter.CUSTOM_BASE_PACKAGE;
    }

    public String customInformationProviderBeanClassName()
    {
        return WebXmlParameter.CUSTOM_INFORMATION_PROVIDER_BEAN;
    }

    public String customComponentMetaDataExtractorClassName()
    {
        return WebXmlParameter.CUSTOM_COMPONENT_META_DATA_EXTRACTOR;
    }

    public String customValidationParameterExtractorClassName()
    {
        return WebXmlParameter.CUSTOM_VALIDATION_PARAMETER_EXTRACTOR;
    }

    public String customStaticValidationStrategyMappingSource()
    {
        return WebXmlParameter.CUSTOM_STATIC_VALIDATION_STRATEGY_MAPPING;
    }

    public String customComponentInitializerClassName()
    {
        return WebXmlParameter.CUSTOM_COMPONENT_INITIALIZER;
    }

    public String customValidationExceptionInterceptorClassName()
    {
        return WebXmlParameter.CUSTOM_VALIDATION_EXCEPTION_INTERCEPTOR;
    }

    public String customPropertyValidationInterceptorClassName()
    {
        return WebXmlParameter.CUSTOM_PROPERTY_VALIDATION_INTERCEPTOR;
    }

    public String customMetaDataExtractionInterceptorClassName()
    {
        return WebXmlParameter.CUSTOM_META_DATA_EXTRACTION_INTERCEPTOR;
    }

    public ProxyHelper proxyHelper()
    {
        String customProxyHelperClassName = WebXmlParameter.CUSTOM_PROXY_HELPER;

        if (customProxyHelperClassName != null && !"".equals(customProxyHelperClassName))
        {
            return (ProxyHelper) ClassUtils.tryToInstantiateClassForName(customProxyHelperClassName);
        }

        return new DefaultProxyHelper();
    }

    public ProjectStageResolver projectStageResolver()
    {
        return new DefaultProjectStageResolver();
    }

    public Class<? extends ExtValRendererProxy> rendererProxy()
    {
        String proxyClassName = (String) ExtValContext.getContext().getGlobalProperty(ExtValRendererProxy.KEY);

        if (proxyClassName != null && !proxyClassName.endsWith(getClass().getName()))
        {
            Class<? extends ExtValRendererProxy> targetClass = ClassUtils.tryToLoadClassForName(proxyClassName);

            if (targetClass == null)
            {
                throw new IllegalStateException("a custom invalid renderer proxy is configured: " + proxyClassName);
            }

            return targetClass;
        }

        return null;
    }

    public Class violationSeverity()
    {
        return ViolationSeverity.class;
    }

    public String customValidationStrategyToMessageResolverNameMapperClassName()
    {
        return WebXmlParameter.CUSTOM_VALIDATION_STRATEGY_TO_MESSAGE_RESOLVER_NAME_MAPPER;
    }

    public String customMetaDataToValidationStrategyNameMapperClassName()
    {
        return WebXmlParameter.CUSTOM_META_DATA_TO_VALIDATION_STRATEGY_NAME_MAPPER;
    }

    public String customValidationStrategyToMetaDataTransformerNameMapperClassName()
    {
        return WebXmlParameter.CUSTOM_VALIDATION_STRATEGY_TO_META_DATA_TRANSFORMER_NAME_MAPPER;
    }

    public String customMetaDataStorageFilterClassName()
    {
        return WebXmlParameter.CUSTOM_META_DATA_STORAGE_FILTER;
    }

    public String customValidationStrategyFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_VALIDATION_STRATEGY_FACTORY;
    }

    public String customMessageResolverFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_MESSAGE_RESOLVER_FACTORY;
    }

    public String customComponentMetaDataExtractorFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_COMPONENT_META_DATA_EXTRACTOR_FACTORY;
    }

    public String customValidationParameterExtractorFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_VALIDATION_PARAMETER_EXTRACTOR_FACTORY;
    }

    public String customValidationParameterFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_VALIDATION_PARAMETER_FACTORY;
    }

    public String customMetaDataTransformerFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_META_DATA_TRANSFORMER_FACTORY;
    }

    public String customStorageManagerFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_STORAGE_MANAGER_FACTORY;
    }

    public String customFacesMessageFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_FACES_MESSAGE_FACTORY;
    }

    public Class<? extends Annotation> constraintSourceAnnotation()
    {
        return ConstraintSource.class;
    }

    public Class<? extends Annotation> ignoreConstraintSourceAnnotation()
    {
        return IgnoreConstraintSource.class;
    }

    public Class<? extends Annotation> targetPropertyAnnotation()
    {
        return TargetProperty.class;
    }

    public Class<? extends Annotation> targetPropertyIdAnnotation()
    {
        return TargetPropertyId.class;
    }

    public Class<? extends ValidationParameter> disableClientSideValidationValidationParameter()
    {
        return DisableClientSideValidation.class;
    }

    public boolean activateRequiredInitialization()
    {
        return WebXmlParameter.ACTIVATE_REQUIRED_INITIALIZATION != null &&
                "true".equalsIgnoreCase(WebXmlParameter.ACTIVATE_REQUIRED_INITIALIZATION.trim());
    }

    public boolean deactivateDefaultConvention()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_DEFAULT_CONVENTION);
    }

    public boolean deactivateDefaultNameMappers()
    {
        String deactivateDefaultNameMappers = WebXmlParameter.DEACTIVATE_DEFAULT_NAME_MAPPERS;
        return deactivateDefaultNameMappers != null && deactivateDefaultNameMappers.trim().equalsIgnoreCase("true");
    }

    public boolean deactivateElResolver()
    {
        return false;
    }

    public boolean deactivateComponentInitialization()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_COMPONENT_INITIALIZATION);
    }

    public boolean deactivateValidationParameters()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_VALIDATION_PARAMETERS);
    }

    public boolean deactivateRenderKitFactory()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_RENDER_KIT_FACTORY);
    }

    public boolean deactivateRequiredAttributeSupport()
    {
        return activateRequiredInitialization();
    }

    public boolean interpretEmptyStringSubmittedValuesAsNull()
    {
        return !"false".equalsIgnoreCase(WebXmlParameter.INTERPRET_EMPTY_STRING_SUBMITTED_VALUES_AS_NULL);
    }

    public boolean validateEmptyFields()
    {
        return !"false".equalsIgnoreCase(WebXmlParameter.VALIDATE_EMPTY_FIELDS);
    }
}
