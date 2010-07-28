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
    
    /**
     * Retrieve the Core Module Configuration stored in the context.  If this doesn't exists (Normally the
     * ExtValStartUpListener registers version)
     * it returns a new default version. 
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
     * Sets the active Core Module Configuration.
     * @param config The Core Module Configuration to activate
     * @param forceOverride if not forced and there is already a configuration active, It isn't activated 
     * @return Is the Core Module Configuration in the parameter set as active?
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
     * @return Fully qualified name of resource bundle.
     * @see org.apache.myfaces.extensions.validator.core.validation.message.resolver.DefaultValidationErrorMessageResolver
     */
    public abstract String customMessageBundleBaseName();

    /**
     * Define a new base package where custom versions of the ExtVal objects can be found.  By default, there are looked
     * up in the package
     * org.apache.myfaces.extensions.validator.custom
     * 
     * @return package name
     * @see InformationProviderBean#setupCustomizableInformation()
     */
    public abstract String customBasePackage();

    /**
     * Defines the class name for a custom {@link InformationProviderBean}. This class must extend the
     * InformationProviderBean of ExtVal.
     *   
     * @return Fully Qualified Class name of the class to be used as InformationProviderBean 
     * @see InformationProviderBean
     */
    public abstract String customInformationProviderBeanClassName();

    /**
     * Defines the class name of a custom {@link
     * org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor}. The default implementation is
     * DefaultComponentMetaDataExtractor.
     * 
     * @return Fully Qualified Class name of implementation of a MetaDataExtractor.
     * @see org.apache.myfaces.extensions.validator.core.metadata.extractor.DefaultComponentMetaDataExtractorFactory#createWith(java.util.Map<java.lang.String,java.lang.Object>)
     */
    public abstract String customComponentMetaDataExtractorClassName();

    /**
     * Defines the class name of a custom {@link
     * org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractor}. The default
     * implementation is DefaultValidationParameterExtractor.
     * 
     * @return Fully Qualified Class name of implementation of a ValidationParameterExtractor. 
     * 
     * @see org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterExtractorFactory#create()
     */
    public abstract String customValidationParameterExtractorClassName();

    /**
     * Name of the property file that contains the mappings between the validation annotations and the
     * ValidationStrategy. These mappings can be used to override the ExtVal defined ones.
     * 
     * @return Fully qualified name of the properties file with the mappings.
     * @see org.apache.myfaces.extensions.validator.core.validation.strategy.DefaultValidationStrategyFactory#initStaticMappings()
     */
    public abstract String customStaticValidationStrategyMappingSource();

    /**
     * Defines the name of an additional {@link
     * org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer}.
     * 
     * @return Fully qualified class name of the implementation of a ComponentInitializer.
     * @see ExtValContextInvocationOrderAwareInternals#lazyInitComponentInitializers()
     */
    public abstract String customComponentInitializerClassName();

    /**
     * Defines the name of an additional {@link
     * org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor}.
     * 
     * @return Fully qualified class name of the implementation of a ValidationExceptionInterceptor.
     * @see ExtValContextInvocationOrderAwareInternals#lazyInitValidationExceptionInterceptors()
     */
    public abstract String customValidationExceptionInterceptorClassName();

    /**
     * Defines the name of an additional {@link
     * org.apache.myfaces.extensions.validator.core.interceptor.PropertyValidationInterceptor}.
     * 
     * @return Fully qualified class name of the implementation of a PropertyValidationInterceptor.
     * @see ExtValContextInvocationOrderAwareInternals#lazyInitPropertyValidationInterceptors()
     */
    public abstract String customPropertyValidationInterceptorClassName();

    /**
     * Defines the name of an additional {@link
     * org.apache.myfaces.extensions.validator.core.interceptor.MetaDataExtractionInterceptor}.
     * 
     * @return Fully qualified class name of the implementation of a MetaDataExtractionInterceptor.
     * @see ExtValContextInvocationOrderAwareInternals#lazyInitMetaDataExtractionInterceptors()
     */
    public abstract String customMetaDataExtractionInterceptorClassName();

    /*
     * others
     */
    
    /**
     * Defines the {@link ProxyHelper} that should be used by ExtVal. By default, the {@link
     * org.apache.myfaces.extensions.validator.core.proxy.DefaultProxyHelper} is used.
     * The method should never return null.
     * 
     * @return The instance of the ProxyHelper that should be used.
     * @see org.apache.myfaces.extensions.validator.util.ProxyUtils#getProxyHelper()
     */
    public abstract ProxyHelper proxyHelper();

    /**
     * Defines the name {@link ProjectStageResolver} that should be used by ExtVal. By default, the {@link
     * DefaultProjectStageResolver} is used and subclasses are allowed to return null for this method.  In that case,
     * the projectStage Production is returned.
     * 
     * @return The instance of the ProjectStageResolver that should be used.
     * @see ProjectStage#getCurrentProjectStage()
     */
    public abstract ProjectStageResolver projectStageResolver();

    /**
     * Defines the {@link org.apache.myfaces.extensions.validator.core.renderkit.RendererProxy} that should be used by
     * ExtVal.
     * 
     * @return The instance of the RendererProxy that should be used.
     * @see org.apache.myfaces.extensions.validator.core.renderkit.ExtValLazyRendererProxy#getLazyRenderer()
     * @see org.apache.myfaces.extensions.validator.core.renderkit.ExtValRendererWrapper#ExtValRendererWrapper(javax.faces.render.Renderer)
     */
    public abstract Class<? extends ExtValRendererProxy> rendererProxy();

    /*
     * ConstraintSource
     */
    
    /**
     * Defines the annotation which specifies the target class where the validation annotations should be taken from.
     * Such an annotation needs the property value that is of type Class. By default, the
     * {@link org.apache.myfaces.extensions.validator.core.validation.ConstraintSource} annotation is used.
     * 
     * @return Annotation class that identifies where the target class for validations is located.
     * @see org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils#findMappedClass(org.apache.myfaces.extensions.validator.core.storage.PropertyStorage, java.lang.Class, java.lang.String)
     */
    public abstract Class<? extends Annotation> constraintSourceAnnotation();

    /**
     * Defines the annotation which indicates that for the property no validation annotations are searched in the target
     * class, defined by the ConstraintSourceAnnotation method.  By default, the
     * {@link org.apache.myfaces.extensions.validator.core.validation.IgnoreConstraintSource} annotation is used.
     * 
     * @return Annotation class that identifies that no validation targets need to be looked up.
     * @see org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils#getIgnoreConstraintSourceAnnotationImplementation()
     */
    public abstract Class<? extends Annotation> ignoreConstraintSourceAnnotation();

    /**
     * Defines the annotation which indicates which property name (by string name) needs to be used to extract the
     * validation annotations. Such an annotation needs the property value which is of type String. By default, the
     * {@link org.apache.myfaces.extensions.validator.core.validation.TargetProperty} annotation is used.
     * 
     * @return Annotation class indicating the property name to use.
     * @see org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils#getTargetPropertyAnnotationImplementation()
     */
    public abstract Class<? extends Annotation> targetPropertyAnnotation();

    /**
     * Defines the annotation which indicates the property that needs to be used to extract the validation annotations.
     * The property should be annotated by the same annotation specified in the value property.  By default, the
     * {@link org.apache.myfaces.extensions.validator.core.validation.TargetPropertyId} annotation is used.
     * 
     * @return Annotation class indicating the property to use.
     * @see org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils#getTargetPropertyIdAnnotationImplementation()
     */
    public abstract Class<? extends Annotation> targetPropertyIdAnnotation();

    /*
     * validation parameter
     */

    /**
     * Defines the class that is used for the definition of the violation severity. By default it is {@link
     * org.apache.myfaces.extensions.validator.core.validation.parameter.ViolationSeverity}.
     * 
     * @return Class used to identify violation severity
     */
    public abstract Class violationSeverity();

    /**
     * Defines the validation parameter annotation which indicates that the validation information should not be placed
     * on the UIComponents (like the length property)
     *
     * @return Validation parameter annotation for indicating that no validation information needs to be set on the view
     * components.
     * @see org.apache.myfaces.extensions.validator.core.startup.ExtValStartupListener#initDisableClientSideValidationKey()
     */
    public abstract Class<? extends ValidationParameter> disableClientSideValidationValidationParameter();

    /*
     * name mapper
     */
    /**
     * Defines the class name of the custom {@link org.apache.myfaces.extensions.validator.core.mapper.NameMapper} that
     * takes a ValidationStrategy and points to the MessageResolver that goes with it. The class should implement the
     * interface NameMapper<ValidationStrategy>.
     * 
     * @return fully qualified class name of the custom NameMapper to retrieve MessageResolver name for a
     * ValidationStrategy.
     * @see org.apache.myfaces.extensions.validator.core.validation.message.resolver.mapper.CustomConfiguredValidationStrategyToMsgResolverNameMapper#getCustomNameMapperClassName()
     */
    public abstract String customValidationStrategyToMessageResolverNameMapperClassName();

    /**
     * Defines the class name of the custom {@link org.apache.myfaces.extensions.validator.core.mapper.NameMapper} that
     * takes a validation annotation key (=class name) and points to the ValidationStrategy that goes with it. The
     * class should implement the interface NameMapper<String>.
     * 
     * @return fully qualified class name of the custom NameMapper to retrieve ValidationStrategy name for a validation
     * key.
     * @see org.apache.myfaces.extensions.validator.core.validation.strategy.mapper.CustomConfiguredAnnotationToValidationStrategyNameMapper#CustomConfiguredAnnotationToValidationStrategyNameMapper()
     */
    public abstract String customMetaDataToValidationStrategyNameMapperClassName();

    /**
     * Defines the class name of the custom {@link org.apache.myfaces.extensions.validator.core.mapper.NameMapper} that
     * takes a ValidationStrategy and points to the MetaDataTransformer that goes with it. The class should implement
     * the interface NameMapper<ValidationStrategy>.
     * 
     * @return fully qualified class name of the custom NameMapper to retrieve MetaDataTransformer name for a
     * ValidationStrategy.
     * @see org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper.CustomConfiguredValidationStrategyToMetaDataTransformerNameMapper#getCustomNameMapperClassName()
     */
    public abstract String customValidationStrategyToMetaDataTransformerNameMapperClassName();

    /*
     * filter
     */
    /**
     * Defines the name of an additional {@link
     * org.apache.myfaces.extensions.validator.core.storage.MetaDataStorageFilter}.
     *
     * 
     * @return Fully qualified class name of the implementation of a MetaDataStorageFilter.
     * @see org.apache.myfaces.extensions.validator.core.storage.DefaultMetaDataStorage#initFilters()
     */
    public abstract String customMetaDataStorageFilterClassName();

    /*
     * factories
     */

    /**
     * Defines the class name of custom {@link org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory
     * <String,org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy>} which is
     * responsible for creating/supplying the ValidationStrategy implementation for a certain metaData key. The default
     * implementation is DefaultValidationStrategyFactory.
     *
     * @return Fully qualified class name of implementation of a ClassMappingFactory.
     *
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createValidationStrategyFactory()
     */
    public abstract String customValidationStrategyFactoryClassName();

    /**
     * Defines the class name of custom {@link org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory
     * <ValidationStrategy, MessageResolver>} which is responsible for creating/supplying the MessageResolver
     * implementation for a certain Validation Strategy. The default implementation is DefaultMessageResolverFactory.
     *
     * @return Fully qualified class name of implementation of a ClassMappingFactory.
     *
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createMessageResolverFactory()
     */
    public abstract String customMessageResolverFactoryClassName();

    /**
     * Defines the class name of a custom {@link
     * org.apache.myfaces.extensions.validator.core.metadata.extractor.ComponentMetaDataExtractorFactory} which creates
     * the MetaDataExtractor's. The default implementation is DefaultComponentMetaDataExtractorFactory.
     *
     * @return Fully qualified class name of implementation of a ComponentMetaDataExtractorFactory.
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createComponentMetaDataExtractorFactory()
     */
    public abstract String customComponentMetaDataExtractorFactoryClassName();

    /**
     * Defines the class name of a custom {@link
     * org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractorFactory}  which
     * creates the ValidationParameterExtractor's. The default implementation is
     * DefaultValidationParameterExtractorFactory.
     *
     * @return Fully qualified class name of implementation of a ValidationParameterExtractorFactory.
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createValidationParameterExtractorFactory()
     */
    public abstract String customValidationParameterExtractorFactoryClassName();

    /**
     * Defines the class name of custom {@link org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory
     * <Class, Class>} which is responsible for creating/supplying the real Validation Parameter class .
     * The default implementation is DefaultValidationParameterFactory.
     *
     * @return Fully qualified class name of implementation of a ClassMappingFactory.
     *
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createValidationParameterFactory() 
     */
    public abstract String customValidationParameterFactoryClassName();

    /**
     * Defines the class name of custom {@link org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory
     * <ValidationStrategy, MetaDataTransformer>} which is responsible for creating/supplying the MetaDataTransformer
     * for a certain validationStrategy . The default implementation is DefaultMetaDataTransformerFactory.
     *
     * @return Fully qualified class name of implementation of a ClassMappingFactory.
     *
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createMetaDataTransformerFactory()
     */
    public abstract String customMetaDataTransformerFactoryClassName();

    /**
     * Defines the class name of custom {@link org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory
     * <Class, StorageManager>} which is responsible for creating/supplying the StorageManager for
     * a Storage class. The default implementation is DefaultStorageManagerFactory.
     *
     * @return Fully qualified class name of implementation of a ClassMappingFactory.
     *
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createStorageManagerFactory()
     */
    public abstract String customStorageManagerFactoryClassName();

    /**
     * Defines the class name of a custom {@link
     * org.apache.myfaces.extensions.validator.core.factory.FacesMessageFactory} which creates and converts
     * FacesMessage's. The default implementation is DefaultValidationParameterExtractorFactory.
     *
     * @return Fully qualified class name of implementation of a FacesMessageFactory.
     * @see org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder#createFacesMessageFactory()
     */
    public abstract String customFacesMessageFactoryClassName();

    /*
     * activate
     */

    /**
     * Indicates if the ComponentInitializer's should mark the UIComponent as 'required' when the Validation annotations
     * on the property indicate that a value must be entered by the user.
     *
     * @return true when required attribute should be set to true.
     * @see org.apache.myfaces.extensions.validator.core.initializer.component.AbstractHtmlCoreComponentsComponentInitializer#configureComponent(javax.faces.context.FacesContext, javax.faces.component.UIComponent, java.util.Map<java.lang.String,java.lang.Object>)
     */
    public abstract boolean activateRequiredInitialization();

    /*
     * deactivate
     */

    /**
     * Indicates if ExtVal should deactivate the usage of the resource bundle for validation messages specified in the
     * InformationBeanProvider.
     *
     * @return  true when we want to deactivate resource bundle.
     * @see org.apache.myfaces.extensions.validator.core.validation.message.resolver.AbstractValidationErrorMessageResolver#tryToUseMessageBundleConvention(java.lang.String, java.util.Locale)
     */
    //currently just used by AbstractValidationErrorMessageResolver
    public abstract boolean deactivateDefaultConvention();

    /**
     * Indicates if ExtVal should deactivate all the internal defined name mappers. Be aware that doing so, it disrupt
     * normal functionality of extVal when no alternative are defined.
     *
     * @return true when we want to deactivate internal defined name mappers.
     * @see org.apache.myfaces.extensions.validator.core.startup.ExtValStartupListener#initNameMappers()
     */
    public abstract boolean deactivateDefaultNameMappers();

    /**
     * Indicates if ExtVal should not use the internal ELResolver to inspect EL constructs.  If deactivated, a fallback
     * mechanism is used that inspects the EL expression string.
     *
     * @return  true when we want to deactivate internal EL Resolver.
     * @see org.apache.myfaces.extensions.validator.core.el.DefaultELHelper
     */
    public abstract boolean deactivateElResolver();

    /**
     * Indicates that the view components don't need to be initialized using the ExtVal information.  It disables all
     * ComponentInitializers but also the extraction of metaData during 'encoding' (render response phase) of the page.
     *
     * @return true if UI component should not be initialized by ExtVal.
     * @see ExtValContextInternals#isComponentInitializationActivated()
     * @see org.apache.myfaces.extensions.validator.core.interceptor.AbstractValidationInterceptor#isComponentInitializationDeactivated()
     */
    public abstract boolean deactivateComponentInitialization();

    /**
     * Indicates that ExtVal does not need to use a ParameterValidatorExtractor.
     *
     * @return true when no parameter validation extraction need to be done.
     * @see org.apache.myfaces.extensions.validator.util.ExtValUtils#getValidationParameterExtractor()
     */
    public abstract boolean deactivateValidationParameters();

    /**
     * Indicates that ExtVal does not create wrappers for Renderer's.
     * TODO Find out what the possible consequences are if it is deactivated.  Trinidad support module does it by
     * default.
     *
     * @return  True when we want to deactivate usage of Renderer wrappers.
     * @see org.apache.myfaces.extensions.validator.core.renderkit.ExtValRenderKitFactory#checkRenderKitFactoryDeactivation()
     */
    public abstract boolean deactivateRenderKitFactory();

    //there is nothing like DEACTIVATE_DEFAULT_VALIDATION_INTERCEPTOR
    //use ExtValContext.getContext().denyRendererInterceptor(...) within an ExtValStartupListener

    /**
     * Indicates if ExtVal should reset the required attribute of the UIComponent after the 'decode' (apply request
     * parameters) phase. This reset of the required attribute is only done when the setting of the attribute is
     * activated by activateRequiredAttributeSupport.<br/>
     * Remark, this method should be better named as activateResetRequiredAttributeSupport.
     *
     * @return true when we want to remove the required attribute on the UICOMpoent after decoding.
     * @see org.apache.myfaces.extensions.validator.util.ExtValUtils#isRequiredResetActivated()
     */
    public abstract boolean deactivateRequiredAttributeSupport();

    /*
     * spec parameters
     */

    /**
     * Should ExtVal interpret the empty field values on screen as null.  A new general JSF 2.0 parameter.
     *
     * @return true if we want backwards compatible functionality.
     */
    public abstract boolean interpretEmptyStringSubmittedValuesAsNull();

    /**
     * Should ExtVal interpret empty fields.  If set, the required attribute can be placed (if all other requirements
     * are met)on the UIComponent.  Other factors that have an impact on the validation of empty fields is the
     * NullValueAwareValidationStrategy annotation.
     *
     * @return true if we want backwards compatible functionality.
     */
    public abstract boolean validateEmptyFields();
}
