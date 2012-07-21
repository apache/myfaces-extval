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
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.lang.annotation.Annotation;

/**
 * Default ExtVal Core Module Configuration that retrieves most of the values from the Web.xml initialization
 * parameters.
 *
 * @author Gerhard Petracek
 * @since r4
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultExtValCoreConfiguration extends ExtValCoreConfiguration
{
    private static final String GLOBAL_PROPERTY_MODE_INIT_REQUIRED = "mode:init:required";
    private static final String GLOBAL_PROPERTY_MODE_RESET_REQUIRED = "mode:reset:required";

    /**
     * {@inheritDoc}
     * Parameter taken from the Web.xml initialization parameter CUSTOM_MESSAGE_BUNDLE.
     */
    public String customMessageBundleBaseName()
    {
        return WebXmlParameter.CUSTOM_MESSAGE_BUNDLE;
    }

    /**
     * {@inheritDoc}
     * Parameter taken from the Web.xml initialization parameter CUSTOM_BASE_PACKAGE.
     */
    public String customBasePackage()
    {
        return WebXmlParameter.CUSTOM_BASE_PACKAGE;
    }

    /**
     * {@inheritDoc}
     * Parameter taken from the Web.xml initialization parameter CUSTOM_INFORMATION_PROVIDER_BEAN.
     */
    public String customInformationProviderBeanClassName()
    {
        return WebXmlParameter.CUSTOM_INFORMATION_PROVIDER_BEAN;
    }

    /**
     * {@inheritDoc}
     * Parameter taken from the Web.xml initialization parameter CUSTOM_COMPONENT_META_DATA_EXTRACTOR.
     */
    public String customComponentMetaDataExtractorClassName()
    {
        return WebXmlParameter.CUSTOM_COMPONENT_META_DATA_EXTRACTOR;
    }

    /**
     * {@inheritDoc}
     * Parameter taken from the Web.xml initialization parameter CUSTOM_VALIDATION_PARAMETER_EXTRACTOR.
     */
    public String customValidationParameterExtractorClassName()
    {
        return WebXmlParameter.CUSTOM_VALIDATION_PARAMETER_EXTRACTOR;
    }

    /**
     * {@inheritDoc}
     * Parameter taken from the Web.xml initialization parameter  CUSTOM_STATIC_VALIDATION_STRATEGY_MAPPING.
     */
    public String customStaticValidationStrategyMappingSource()
    {
        return WebXmlParameter.CUSTOM_STATIC_VALIDATION_STRATEGY_MAPPING;
    }

    /**
     * {@inheritDoc}
     * Parameter taken from the Web.xml initialization parameter CUSTOM_COMPONENT_INITIALIZER.
     */
    public String customComponentInitializerClassName()
    {
        return WebXmlParameter.CUSTOM_COMPONENT_INITIALIZER;
    }

    /**
     * {@inheritDoc}
     * Parameter taken from the Web.xml initialization parameter CUSTOM_VALIDATION_EXCEPTION_INTERCEPTOR.
     */
    public String customValidationExceptionInterceptorClassName()
    {
        return WebXmlParameter.CUSTOM_VALIDATION_EXCEPTION_INTERCEPTOR;
    }

    /**
     * {@inheritDoc}
     * Parameter taken from the Web.xml initialization parameter CUSTOM_PROPERTY_VALIDATION_INTERCEPTOR.
     */
    public String customPropertyValidationInterceptorClassName()
    {
        return WebXmlParameter.CUSTOM_PROPERTY_VALIDATION_INTERCEPTOR;
    }

    /**
     * {@inheritDoc}
     * Parameter taken from the Web.xml initialization parameter CUSTOM_META_DATA_EXTRACTION_INTERCEPTOR.
     */
    public String customMetaDataExtractionInterceptorClassName()
    {
        return WebXmlParameter.CUSTOM_META_DATA_EXTRACTION_INTERCEPTOR;
    }

    /**
     * {@inheritDoc}
     * Parameter taken from the Web.xml initialization parameter CUSTOM_META_DATA_EXTRACTION_INTERCEPTOR. When no
     * value is specified, the {@link DefaultProxyHelper} is returned.
     */
    @ToDo(value = Priority.MEDIUM, description = "The tryToInstantiateClassForName can return a null value which isn't "
            + "handled correctly afterwards.  We should throw an exception like in rendererProxy method.")
    public ProxyHelper proxyHelper()
    {
        String customProxyHelperClassName = WebXmlParameter.CUSTOM_PROXY_HELPER;

        if (customProxyHelperClassName != null && !"".equals(customProxyHelperClassName))
        {
            return (ProxyHelper) ClassUtils.tryToInstantiateClassForName(customProxyHelperClassName);
        }

        return new DefaultProxyHelper();
    }

    /**
     * {@inheritDoc}
     * Returns always the DefaultProjectStageResolver.
     */
    public ProjectStageResolver projectStageResolver()
    {
        return new DefaultProjectStageResolver();
    }

    /**
     * {@inheritDoc}
     * Returns the ExtValRendererProxy class which is configured in the global property of ExtVal. When no property
     * defined, null is returned (which is correctly handled by the calling methods) or an exception can be thrown
     * when the global property contains an invalid class name.
     */
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

    /**
     * {@inheritDoc}
     * Returns always the ViolationSeverity.
     */
    public Class violationSeverity()
    {
        return ViolationSeverity.class;
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter CUSTOM_VALIDATION_STRATEGY_TO_MESSAGE_RESOLVER_NAME_MAPPER.
     */
    public String customValidationStrategyToMessageResolverNameMapperClassName()
    {
        return WebXmlParameter.CUSTOM_VALIDATION_STRATEGY_TO_MESSAGE_RESOLVER_NAME_MAPPER;
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter CUSTOM_META_DATA_TO_VALIDATION_STRATEGY_NAME_MAPPER.
     */
    public String customMetaDataToValidationStrategyNameMapperClassName()
    {
        return WebXmlParameter.CUSTOM_META_DATA_TO_VALIDATION_STRATEGY_NAME_MAPPER;
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter
     * CUSTOM_VALIDATION_STRATEGY_TO_META_DATA_TRANSFORMER_NAME_MAPPER.
     */
    public String customValidationStrategyToMetaDataTransformerNameMapperClassName()
    {
        return WebXmlParameter.CUSTOM_VALIDATION_STRATEGY_TO_META_DATA_TRANSFORMER_NAME_MAPPER;
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter CUSTOM_META_DATA_STORAGE_FILTER.
     */
    public String customMetaDataStorageFilterClassName()
    {
        return WebXmlParameter.CUSTOM_META_DATA_STORAGE_FILTER;
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter CUSTOM_VALIDATION_STRATEGY_FACTORY.
     */
    public String customValidationStrategyFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_VALIDATION_STRATEGY_FACTORY;
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter CUSTOM_MESSAGE_RESOLVER_FACTORY.
     */
    public String customMessageResolverFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_MESSAGE_RESOLVER_FACTORY;
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter CUSTOM_COMPONENT_META_DATA_EXTRACTOR_FACTORY.
     */
    public String customComponentMetaDataExtractorFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_COMPONENT_META_DATA_EXTRACTOR_FACTORY;
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter CUSTOM_VALIDATION_PARAMETER_EXTRACTOR_FACTORY.
     */
    public String customValidationParameterExtractorFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_VALIDATION_PARAMETER_EXTRACTOR_FACTORY;
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter CUSTOM_VALIDATION_PARAMETER_FACTORY.
     */
    public String customValidationParameterFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_VALIDATION_PARAMETER_FACTORY;
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter CUSTOM_META_DATA_TRANSFORMER_FACTORY.
     */
    public String customMetaDataTransformerFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_META_DATA_TRANSFORMER_FACTORY;
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter CUSTOM_STORAGE_MANAGER_FACTORY.
     */
    public String customStorageManagerFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_STORAGE_MANAGER_FACTORY;
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter CUSTOM_FACES_MESSAGE_FACTORY.
     */
    public String customFacesMessageFactoryClassName()
    {
        return WebXmlParameter.CUSTOM_FACES_MESSAGE_FACTORY;
    }

    /**
     * {@inheritDoc}
     * Returns the {@link ConstraintSource} class as annotation for the Constraint Source feature.  
     */
    public Class<? extends Annotation> constraintSourceAnnotation()
    {
        return ConstraintSource.class;
    }

    /**
     * {@inheritDoc}
     * Returns the {@link IgnoreConstraintSource} class as annotation for the ignore feature of the Constraint Source.
     */
    public Class<? extends Annotation> ignoreConstraintSourceAnnotation()
    {
        return IgnoreConstraintSource.class;
    }

    /**
     * {@inheritDoc}
     * Returns the {@link TargetProperty} class as annotation for the Constraint Source target property feature.
     */
    public Class<? extends Annotation> targetPropertyAnnotation()
    {
        return TargetProperty.class;
    }

    /**
     * {@inheritDoc}
     * Returns the {@link TargetPropertyId} class as annotation for the Constraint Source target property feature.
     */
    public Class<? extends Annotation> targetPropertyIdAnnotation()
    {
        return TargetPropertyId.class;
    }

    /**
     * {@inheritDoc}
     * Returns the {@link DisableClientSideValidation} class as indicator for this feature.
     */
    public Class<? extends ValidationParameter> disableClientSideValidationValidationParameter()
    {
        return DisableClientSideValidation.class;
    }

    /**
     * {@inheritDoc}
     * First a global parameter is checked to see if the Module or Add-on hasn't overruled the value of the
     * Web.xml initialization parameter ACTIVATE_REQUIRED_INITIALIZATION.  If overruled, this value is taken, otherwise
     * the initialization parameter value.  By default, the method return false if there are no values specified.
     */
    public boolean activateRequiredInitialization()
    {
        Boolean globalProperty = (Boolean)ExtValContext.getContext().getGlobalProperty(
                GLOBAL_PROPERTY_MODE_INIT_REQUIRED);

        if(globalProperty != null)
        {
            return globalProperty;
        }

        return WebXmlParameter.ACTIVATE_REQUIRED_INITIALIZATION != null &&
                "true".equalsIgnoreCase(WebXmlParameter.ACTIVATE_REQUIRED_INITIALIZATION);
    }

    /**
     * Module and add-on writers can use this method to overrule the value of the parameter
     * activateRequiredInitialization. it is also useful for them to specify a certain parameter value without the need
     * for a web.xml initialization parameter.
     * 
     * @param value The value we want to give the parameter
     * @param forceOverride do we force overriding of another value set by a call to this overrule method.
     */
    public static void overruleActivateRequiredInitialization(Boolean value, boolean forceOverride)
    {
        ExtValContext.getContext().addGlobalProperty(GLOBAL_PROPERTY_MODE_INIT_REQUIRED, value, forceOverride);
    }
    
    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter DEACTIVATE_DEFAULT_CONVENTION.
     */
    public boolean deactivateDefaultConvention()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_DEFAULT_CONVENTION);
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter DEACTIVATE_DEFAULT_NAME_MAPPERS.
     */
    public boolean deactivateDefaultNameMappers()
    {
        String deactivateDefaultNameMappers = WebXmlParameter.DEACTIVATE_DEFAULT_NAME_MAPPERS;
        return deactivateDefaultNameMappers != null && deactivateDefaultNameMappers.equalsIgnoreCase("true");
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter DEACTIVATE_EL_RESOLVER.
     */
    public boolean deactivateElResolver()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_EL_RESOLVER);
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter DEACTIVATE_COMPONENT_INITIALIZATION.
     */
    public boolean deactivateComponentInitialization()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_COMPONENT_INITIALIZATION);
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter DEACTIVATE_VALIDATION_PARAMETERS.
     */
    public boolean deactivateValidationParameters()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_VALIDATION_PARAMETERS);
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter DEACTIVATE_RENDER_KIT_FACTORY.
     */
    public boolean deactivateRenderKitFactory()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_RENDER_KIT_FACTORY);
    }

    /**
     * {@inheritDoc}
     * First a global parameter is checked to see if some Module or Add-on hasn't overruled the value of the
     * Web.xml initialization parameter DEACTIVATE_REQUIRED_ATTRIBUTE_SUPPORT.  If overruled, this value is taken,
     * otherwise the initialization parameter value.  By default, the method return false if there are no values
     * specified.
     */
    public boolean deactivateRequiredAttributeSupport()
    {
        Boolean globalProperty = (Boolean)ExtValContext.getContext().getGlobalProperty(
                GLOBAL_PROPERTY_MODE_RESET_REQUIRED);

        if(globalProperty != null)
        {
            return globalProperty;
        }

        return WebXmlParameter.DEACTIVATE_REQUIRED_ATTRIBUTE_SUPPORT != null &&
                "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_REQUIRED_ATTRIBUTE_SUPPORT);
    }
    
    /**
     * Module and add-on writers can use this method to overrule the value of the parameter
     * deactivateRequiredAttributeSupport. It is also useful for them to specify a certain parameter value without the
     * need for a web.xml initialization parameter.
     * 
     * @param value The value we want to give the parameter
     * @param forceOverride do we force overriding of another value set by a call to this overrule method.
     */
    public static void overruleDeactivateRequiredAttributeSupport(Boolean value, boolean forceOverride)
    {
        ExtValContext.getContext().addGlobalProperty(GLOBAL_PROPERTY_MODE_RESET_REQUIRED, value, forceOverride);
    }
    
    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization (JSF 2) parameter
     * avax.faces.INTERPRET_EMPTY_STRING_SUBMITTED_VALUES_AS_NULL.
     */
    public boolean interpretEmptyStringSubmittedValuesAsNull()
    {
        return !"false".equalsIgnoreCase(WebXmlParameter.INTERPRET_EMPTY_STRING_SUBMITTED_VALUES_AS_NULL);
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization (JSF 2) parameter javax.faces.VALIDATE_EMPTY_FIELDS.
     */
    public boolean validateEmptyFields()
    {
        return !"false".equalsIgnoreCase(WebXmlParameter.VALIDATE_EMPTY_FIELDS);
    }

    /**
     * {@inheritDoc}
     * Value taken from the Web.xml initialization parameter ACTIVATE_MARKUP_META_DATA.
     */
    @Override
    public boolean activateMarkupMetaData()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.ACTIVATE_MARKUP_META_DATA);
    }
}
