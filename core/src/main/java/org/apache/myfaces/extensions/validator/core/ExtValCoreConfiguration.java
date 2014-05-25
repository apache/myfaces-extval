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
 * ExtVal Core Module configuration.
 * 'custom' as prefix is used for 'optional' configurations. That means
 * if a method returns null ExtVal uses a different approach to find an implementation e.g. via a naming convention
 * -> all other methods aren't allowed to return null if there is no additional rule.
 *
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
     * Don't access ExtValContext during initialization of the class. E.g. OpenWebBeans initializes all classes during
     * startup of the WebContainer.
     * ({@link ExtValContext} constructor tries to access Web.xml parameters through
     * {@link javax.faces.context.FacesContext} which isn't available during the classpath-scanning.)
     *
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
    
    /**
     * Returns the configuration for the core-module stored in the context.
     * If this doesn't exists (usually a startup-listener registers a (custom) implementation),
     * it returns a new instance of the default implementation.
     * 
     * @return The active ExtVal Core Module Configuration 
     */
    public static ExtValCoreConfiguration get()
    {
        ExtValCoreConfiguration moduleConfig = getExtValContext().getModuleConfiguration(ExtValCoreConfiguration.class);

        if(moduleConfig == null)
        {
            LOGGER.fine(MISSING_MODULE_CONFIG_MESSAGE);
        }
        return moduleConfig != null ? moduleConfig : new DefaultExtValCoreConfiguration();
    }

    /**
     * Sets a new configuration for the core-module
     *
     * @param config The new configuration for the core-module
     * @param forceOverride use true to replace an existing configuration
     * @return true if the new config was registered successfully
     */
    @UsageInformation(UsageCategory.INTERNAL)
    public static boolean use(ExtValCoreConfiguration config, boolean forceOverride)
    {
        return getExtValContext().addModuleConfiguration(ExtValCoreConfiguration.class, config, forceOverride);
    }

    /*
     * web.xml config
     */
    /**
     * The name of the Resource bundle used for looking up resource keys of validation messages.
     * 
     * @return Fully qualified name of a custom resource bundle.
     * @see org.apache.myfaces.extensions.validator.core.validation.message.resolver.DefaultValidationErrorMessageResolver
     */
    public abstract String customMessageBundleBaseName();

    /**
     * Define a new base package where custom versions of the ExtVal artifacts can be found.
     * (Default value: org.apache.myfaces.extensions.validator.custom)
     *
     * @see InformationProviderBean#setupCustomizableInformation()
     *
     * @return package name
     */
    public abstract String customBasePackage();

    /**
     * Defines the class name for a custom {@link InformationProviderBean}. This class must extend
     * {@link InformationProviderBean}.
     *   
     * @return Fully qualified class name of the class to be used as {@link InformationProviderBean}
     */
    public abstract String customInformationProviderBeanClassName();

    /**
     * Defines the class name of a custom
     * {@link org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor}.
     * (Default implementation:
     * {@link org.apache.myfaces.extensions.validator.core.metadata.extractor.DefaultComponentMetaDataExtractor}).
     *
     * @see org.apache.myfaces.extensions.validator.core.metadata.extractor.DefaultComponentMetaDataExtractorFactory#createWith(java.util.Map<java.lang.String,java.lang.Object>)
     *
     * @return Fully qualified class name of the class to be used as
     * {@link org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor}.
     */
    public abstract String customComponentMetaDataExtractorClassName();

    /**
     * Defines the class name of a custom {@link
     * org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractor}.
     * (Default implementation:
     * {@link org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterExtractor}.
     *
     * @see org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterExtractorFactory#create()
     *
     * @return Fully qualified class name of the class to be used as
     * {@link org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractor).
     */
    public abstract String customValidationParameterExtractorClassName();

    /**
     * Name of the optional property file which contains the mappings between ExtVal constraints and the
     * {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy}s for validating them.
     * These mappings can be used to overrule all other configurations.
     *
     * @see org.apache.myfaces.extensions.validator.core.validation.strategy.DefaultValidationStrategyFactory#initStaticMappings()
     *
     * @return Fully qualified name of the property file which contains the mappings.
     */
    public abstract String customStaticValidationStrategyMappingSource();

    /**
     * Defines the name of a custom
     * {@link org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer}.
     *
     * @see ExtValContextInvocationOrderAwareInternals#lazyInitComponentInitializers()
     *
     * @return Fully qualified class name of the class to be used as
     * {@link org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer}.
     */
    public abstract String customComponentInitializerClassName();

    /**
     * Defines the name of a custom
     * {@link org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor}.
     *
     * @see ExtValContextInvocationOrderAwareInternals#lazyInitValidationExceptionInterceptors()
     *
     * @return Fully qualified class name of the class to be used as
     * {@link org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor}.
     */
    public abstract String customValidationExceptionInterceptorClassName();

    /**
     * Defines the name of a custom
     * {@link org.apache.myfaces.extensions.validator.core.interceptor.PropertyValidationInterceptor}.
     *
     * @see ExtValContextInvocationOrderAwareInternals#lazyInitPropertyValidationInterceptors()
     *
     * @return Fully qualified class name of the class to be used as
     * {@link org.apache.myfaces.extensions.validator.core.interceptor.PropertyValidationInterceptor}.
     */
    public abstract String customPropertyValidationInterceptorClassName();

    /**
     * Defines the name of a custom
     * {@link org.apache.myfaces.extensions.validator.core.interceptor.MetaDataExtractionInterceptor}.
     *
     * @see ExtValContextInvocationOrderAwareInternals#lazyInitMetaDataExtractionInterceptors()
     *
     * @return Fully qualified class name of the class to be used as
     * {@link org.apache.myfaces.extensions.validator.core.interceptor.MetaDataExtractionInterceptor}.
     */
    public abstract String customMetaDataExtractionInterceptorClassName();

    /*
     * others
     */
    
    /**
     * Defines the {@link ProxyHelper} that should be used by ExtVal.
     * (Default implementation: {@link org.apache.myfaces.extensions.validator.core.proxy.DefaultProxyHelper}).
     * The method should never return null.
     *
     * @see org.apache.myfaces.extensions.validator.util.ProxyUtils#getProxyHelper()
     *
     * @return The instance of the {@link ProxyHelper} that should be used.
     */
    public abstract ProxyHelper proxyHelper();

    /**
     * Defines the {@link ProjectStageResolver} that should be used by ExtVal.
     * (Default implementation: {@link DefaultProjectStageResolver}
     * If the method returns null, the project-stage 'Production' will be used.
     *
     * @see ProjectStage#getCurrentProjectStage()
     *
     * @return The instance of the ProjectStageResolver that should be used.
     */
    public abstract ProjectStageResolver projectStageResolver();

    /**
     * Defines an optional {@link org.apache.myfaces.extensions.validator.core.renderkit.RendererProxy}
     * that should be used by ExtVal.
     *
     * @see org.apache.myfaces.extensions.validator.core.renderkit.ExtValLazyRendererProxy#getLazyRenderer()
     * @see org.apache.myfaces.extensions.validator.core.renderkit.ExtValRendererWrapper#ExtValRendererWrapper(javax.faces.render.Renderer)
     *
     * @return The instance of the {@link org.apache.myfaces.extensions.validator.core.renderkit.RendererProxy}
     * that should be used.
     */
    public abstract Class<? extends ExtValRendererProxy> rendererProxy();

    /*
     * ConstraintSource
     */
    
    /**
     * Returns a class of an annotation which should be used as
     * {@link org.apache.myfaces.extensions.validator.core.validation.ConstraintSource}
     * (use a custom implementation, if it is required to keep implementations independent of ExtVal)
     *
     * @see org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils#findMappedClass(org.apache.myfaces.extensions.validator.core.storage.PropertyStorage, java.lang.Class, java.lang.String)
     *
     * @return Annotation class of the alternative implementation
     */
    public abstract Class<? extends Annotation> constraintSourceAnnotation();

    /**
     * Returns a class of an annotation which should be used as
     * {@link org.apache.myfaces.extensions.validator.core.validation.IgnoreConstraintSource}
     * (use a custom implementation, if it is required to keep implementations independent of ExtVal)
     *
     * @see org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils#getIgnoreConstraintSourceAnnotationImplementation()
     *
     * @return Annotation class of the alternative implementation
     */
    public abstract Class<? extends Annotation> ignoreConstraintSourceAnnotation();

    /**
     * Returns a class of an annotation which should be used as
     * {@link org.apache.myfaces.extensions.validator.core.validation.TargetProperty}
     * (use a custom implementation, if it is required to keep implementations independent of ExtVal)
     *
     * @see org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils#getTargetPropertyAnnotationImplementation()
     *
     * @return Annotation class of the alternative implementation
     */
    public abstract Class<? extends Annotation> targetPropertyAnnotation();

    /**
     * Returns a class of an annotation which should be used as
     * {@link org.apache.myfaces.extensions.validator.core.validation.TargetPropertyId}
     * (use a custom implementation, if it is required to keep implementations independent of ExtVal)
     *
     * @see org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils#getTargetPropertyIdAnnotationImplementation()
     *
     * @return Annotation class of the alternative implementation
     */
    public abstract Class<? extends Annotation> targetPropertyIdAnnotation();

    /*
     * validation parameter
     */

    /**
     * Returns the class which should be used as violation severity.
     * (Default implementation:
     * {@link org.apache.myfaces.extensions.validator.core.validation.parameter.ViolationSeverity}).
     * 
     * @return class which should be used as violation severity
     */
    public abstract Class violationSeverity();

    /**
     * Defines the validation parameter annotation which indicates that the validation information should not be
     * transferred to the {@link javax.faces.component.UIComponent}.
     *
     * @see org.apache.myfaces.extensions.validator.core.startup.ExtValStartupListener#initDisableClientSideValidationKey()
     *
     * @return Validation parameter annotation for indicating that no validation information needs to be set on the
     * ui-component.
     */
    public abstract Class<? extends ValidationParameter> disableClientSideValidationValidationParameter();

    /*
     * name mapper
     */

    /**
     * Defines the name of a custom
     * {@link org.apache.myfaces.extensions.validator.core.mapper.NameMapper} that
     * takes a ValidationStrategy and points to the
     * {@link org.apache.myfaces.extensions.validator.core.validation.message.resolver.MessageResolver}
     * that is linked to it.
     * (The class should implement the interface NameMapper<ValidationStrategy>.)
     *
     * @see org.apache.myfaces.extensions.validator.core.validation.message.resolver.mapper.CustomConfiguredValidationStrategyToMsgResolverNameMapper#getCustomNameMapperClassName()
     *
     * @return fully qualified class name of the custom NameMapper to retrieve MessageResolver name for a given
     * ValidationStrategy.
     */
    public abstract String customValidationStrategyToMessageResolverNameMapperClassName();

    /**
     * Defines the name of the custom
     * {@link org.apache.myfaces.extensions.validator.core.mapper.NameMapper} that
     * takes a constraint-key and points to the
     * {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy}
     * that is linked to it.
     * (The class should implement the interface NameMapper<String>.)
     *
     * @see org.apache.myfaces.extensions.validator.core.validation.strategy.mapper.CustomConfiguredAnnotationToValidationStrategyNameMapper#CustomConfiguredAnnotationToValidationStrategyNameMapper()
     *
     * @return fully qualified class name of the custom NameMapper to retrieve ValidationStrategy name for a given
     * constraint-key.
     */
    public abstract String customMetaDataToValidationStrategyNameMapperClassName();

    /**
     * Defines the name of the custom
     * {@link org.apache.myfaces.extensions.validator.core.mapper.NameMapper} that
     * takes a ValidationStrategy and points to the
     * {@link org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer}
     * that is linked to it.
     * (The class should implement the interface NameMapper<ValidationStrategy>.)
     *
     * @see org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper.CustomConfiguredValidationStrategyToMetaDataTransformerNameMapper#getCustomNameMapperClassName()
     *
     * @return fully qualified class name of the custom NameMapper to retrieve MetaDataTransformer name for a given
     * ValidationStrategy.
     */
    public abstract String customValidationStrategyToMetaDataTransformerNameMapperClassName();

    /*
     * filter
     */

    /**
     * Defines the name of a custom
     * {@link org.apache.myfaces.extensions.validator.core.storage.MetaDataStorageFilter}.
     *
     * @see org.apache.myfaces.extensions.validator.core.storage.DefaultMetaDataStorage#initFilters()
     *
     * @return Fully qualified class name of the class to be used as
     * {@link org.apache.myfaces.extensions.validator.core.storage.MetaDataStorageFilter}.
     */
    public abstract String customMetaDataStorageFilterClassName();

    /*
     * factories
     */

    /**
     * Defines the name of a custom
     * {@link org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory
     * <String,org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy>} which is
     * responsible for creating a
     * {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy}
     * for a given metaData-key.
     * (Default implementation:
     * {@link org.apache.myfaces.extensions.validator.core.validation.strategy.DefaultValidationStrategyFactory}.)
     *
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createValidationStrategyFactory()
     *
     * @return Fully qualified class name of the class to be used as
     * {@link org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory}
     */
    public abstract String customValidationStrategyFactoryClassName();

    /**
     * Defines the name of a custom
     * {@link org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory
     * <ValidationStrategy, MessageResolver>}
     * which is responsible for creating the
     * {@link org.apache.myfaces.extensions.validator.core.validation.message.resolver.MessageResolver}
     * for a given {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy}.
     * (Default implementation:
     * {@link org.apache.myfaces.extensions.validator.core.validation.message.resolver.DefaultMessageResolverFactory}.)
     *
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createMessageResolverFactory()
     *
     * @return Fully qualified class name of the class to be used as
     * {@link org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory}
     */
    public abstract String customMessageResolverFactoryClassName();

    /**
     * Defines the name of a custom
     * {@link org.apache.myfaces.extensions.validator.core.metadata.extractor.ComponentMetaDataExtractorFactory}
     * which creates {@link org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor}s.
     * (Default implementation:
     * {@link org.apache.myfaces.extensions.validator.core.metadata.extractor.DefaultComponentMetaDataExtractorFactory}
     * .)
     *
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createComponentMetaDataExtractorFactory()
     *
     * @return Fully qualified class name of the class to be used as
     * {@link org.apache.myfaces.extensions.validator.core.metadata.extractor.ComponentMetaDataExtractorFactory}
     */
    public abstract String customComponentMetaDataExtractorFactoryClassName();

    /**
     * Defines the name of a custom
     * {@link org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractorFactory}
     * which creates
     * {@link org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractor}s.
     * (Default implementation:
     * {@link
     * org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterExtractorFactory}.)
     *
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createValidationParameterExtractorFactory()
     *
     * @return Fully qualified class name of the class to be used as
     * {@link org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractorFactory}
     */
    public abstract String customValidationParameterExtractorFactoryClassName();

    /**
     * Defines the name of a custom
     * {@link org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory <Class, Class>}
     * which is responsible for creating the final Validation Parameter class.
     * (Default implementation:
     * {@link org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterFactory}.)
     *
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createValidationParameterFactory()
     *
     * @return Fully qualified class name of the class to be used as
     * {@link org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory}
     */
    public abstract String customValidationParameterFactoryClassName();

    /**
     * Defines the name of a custom
     * {@link org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory
     * <ValidationStrategy, MetaDataTransformer>}
     * which is responsible for creating a
     * {@link org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer}
     * for a given
     * {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy}.
     * (Default implementation:
     * {@link org.apache.myfaces.extensions.validator.core.metadata.transformer.DefaultMetaDataTransformerFactory}.)
     *
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createMetaDataTransformerFactory()
     *
     * @return Fully qualified class name of the class to be used as
     * {@link org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory}
     */
    public abstract String customMetaDataTransformerFactoryClassName();

    /**
     * Defines the name of a custom
     * {@link org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory
     * <Class, StorageManager>} which is responsible for creating the
     * {@link org.apache.myfaces.extensions.validator.core.storage.StorageManager} for
     * a given storage class.
     * (Default implementation:
     * {@link org.apache.myfaces.extensions.validator.core.storage.DefaultStorageManagerFactory}.)
     *
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createStorageManagerFactory()
     *
     * @return Fully qualified class name of implementation of a ClassMappingFactory.
     */
    public abstract String customStorageManagerFactoryClassName();

    /**
     * Defines the name of a custom
     * {@link org.apache.myfaces.extensions.validator.core.factory.FacesMessageFactory}
     * which creates and converts {@link javax.faces.application.FacesMessage}s
     * (Default implementation:
     * {@link
     * org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterExtractorFactory}.)
     *
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createFacesMessageFactory()
     *
     * @return Fully qualified class name of the class to be used as
     * {@link org.apache.myfaces.extensions.validator.core.factory.FacesMessageFactory}
     */
    public abstract String customFacesMessageFactoryClassName();

    /*
     * activate
     */

    /**
     * Indicates if the ComponentInitializer's should mark {@link javax.faces.component.UIComponent}s as 'required'
     * if an equivalent constraint is hosted by the referenced property.
     *
     * @see org.apache.myfaces.extensions.validator.core.initializer.component.AbstractHtmlCoreComponentsComponentInitializer#configureComponent(javax.faces.context.FacesContext, javax.faces.component.UIComponent, java.util.Map<java.lang.String,java.lang.Object>)
     *
     * @return true if the required attribute should be honored, false otherwise.
     */
    public abstract boolean activateRequiredInitialization();

    /*
     * deactivate
     */

    /**
     * Indicates if ExtVal should deactivate the default convention.
     *
     * @return true if the default convention should be deactivated, false otherwise
     */
    //currently just used by AbstractValidationErrorMessageResolver
    public abstract boolean deactivateDefaultConvention();

    /**
     * Indicates if ExtVal should deactivate all the internal defined name mappers.
     * (Be aware that ExtVal will fail, if no alternatives are defined.)
     *
     * @see org.apache.myfaces.extensions.validator.core.startup.ExtValStartupListener#initNameMappers()
     *
     * @return true when we want to deactivate internal defined name mappers.
     */
    public abstract boolean deactivateDefaultNameMappers();

    /**
     * Defines if ExtVal should fallback to an alternative (but deprecated) approach for inspecting EL-expressions.
     *
     * @return  true when we want to deactivate internal EL Resolver.
     * @see org.apache.myfaces.extensions.validator.core.el.DefaultELHelper
     */
    @Deprecated
    public abstract boolean deactivateElResolver();

    /**
     * Defines if the mechanism of component initialization (before the rendering process) should be deactivated.
     *
     * @see ExtValContextInternals#isComponentInitializationActivated()
     * @see org.apache.myfaces.extensions.validator.core.interceptor.AbstractValidationInterceptor#isComponentInitializationDeactivated()
     *
     * @return true if {@link javax.faces.component.UIComponent}s should not be initialized by ExtVal, false otherwise.
     */
    public abstract boolean deactivateComponentInitialization();

    /**
     * Defines if the mechanism of generic validation parameters should be deactivated.
     *
     * @see org.apache.myfaces.extensions.validator.util.ExtValUtils#getValidationParameterExtractor()
     *
     * @return true if validation parameter extraction should be skipped, false otherwise
     */
    public abstract boolean deactivateValidationParameters();

    /**
     * Defines if the default entry point of ExtVal should be deactivated.
     * (Attention: in case of mojarra you have to use the vm-parameter:
     * org.apache.myfaces.extensions.validator.DEACTIVATE_RENDER_KIT_FACTORY)
     *
     * @see org.apache.myfaces.extensions.validator.core.renderkit.ExtValRenderKitFactory#checkRenderKitFactoryDeactivation()
     *
     * @return true for deactivating ExtVal, false otherwise
     */
    public abstract boolean deactivateRenderKitFactory();

    //there is nothing like DEACTIVATE_DEFAULT_VALIDATION_INTERCEPTOR
    //use ExtValContext.getContext().denyRendererInterceptor(...) within an ExtValStartupListener

    /**
     * Defines if ExtVal should reset the required attribute of a {@link javax.faces.component.UIComponent}
     * after the decoding the component. (It's needed for special use-cases.)
     *
     * @see org.apache.myfaces.extensions.validator.util.ExtValUtils#isRequiredResetActivated()
     *
     * @return true if ExtVal should set the required attribute to false, false otherwise
     */
    public abstract boolean deactivateRequiredAttributeSupport();

    /*
     * supported spec parameters
     */

    /**
     * Defines if ExtVal should convert empty strings to null (just for the validation process).
     * (Introduced by JSF 2.0)
     * Compared to std. JSF it's activated by default!
     *
     * @return false for using the default behavior of JSF 2.0
     */
    public abstract boolean interpretEmptyStringSubmittedValuesAsNull();

    /**
     * Defines if ExtVal should validate empty fields.
     *
     * Please also have a look at the
     * {@link org.apache.myfaces.extensions.validator.core.validation.NullValueAwareValidationStrategy} annotation.
     *
     * (Introduced by JSF 2.0)
     * Compared to std. JSF it's activated by default!
     *
     * @return false for using the default behavior of JSF 2.0
     */
    public abstract boolean validateEmptyFields();

    /**
     * @since r6
     */

    /**
     * Per default component initialization overrules properties of the component.
     * With activating markup meta-data it's possible to overrule the meta-data of the constraints with the meta-data
     * of the component.
     * @return true to overrule constraint meta-data with meta-data provided by the component, false otherwise
     */
    public abstract boolean activateMarkupMetaData();

    /**
     * @since r7
     */

    public abstract boolean deactivateActionBasedGroupValidation();

    /**
     * @since r8
     */

    /**
     * Per default ExtVal creates a custom {@link javax.faces.component.UIViewRoot} implementation to allow
     * group-validation,... triggered via action-methods. It's possible to restore the default behaviour by
     * deactivating this feature.
     *
     * @return true to deactivate the whole feature, false otherwise
     */
    public abstract boolean deactivateViewRootInterceptor();

    public abstract String customViewRootInterceptorClassName();

    public abstract boolean deactivateActionBasedGroupValidationViaAjax();
}
