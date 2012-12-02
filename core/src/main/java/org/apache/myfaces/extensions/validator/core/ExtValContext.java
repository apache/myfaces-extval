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

import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryFinder;
import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationNames;
import org.apache.myfaces.extensions.validator.core.interceptor.MetaDataExtractionInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.PropertyValidationInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.RendererInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.recorder.ProcessedInformationRecorder;
import org.apache.myfaces.extensions.validator.core.validation.SkipValidationEvaluator;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ViolationSeverityInterpreter;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ClassUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import static org.apache.myfaces.extensions.validator.util.WebXmlUtils.getInitParameter;

/**
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public class ExtValContext
{
    private final Logger logger = Logger.getLogger(getClass().getName());

    private static ExtValContext extValContext;

    //don't try to resolve it dynamically e.g. via InformationProviderBean - there's a mojarra issue
    private static final String CUSTOM_EXTVAL_CONTEXT_CLASS_NAME =
            ExtValContext.class.getName().replace(".core.", ".custom.");

    private static final String CUSTOM_EXTVAL_MODULE_CONFIGURATION_RESOLVER_CLASS_NAME =
            ExtValModuleConfigurationResolver.class.getName().replace(".core.", ".custom.");

    private ViolationSeverityInterpreter violationSeverityInterpreter;
    private FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
    private Map<String, RendererInterceptor> rendererInterceptors =
            new ConcurrentHashMap<String, RendererInterceptor>();
    private List<String> deniedInterceptors = new CopyOnWriteArrayList<String>();
    private List<ProcessedInformationRecorder> processedInformationRecorders =
            new CopyOnWriteArrayList<ProcessedInformationRecorder>();

    private SkipValidationEvaluator skipValidationEvaluator;

    private Map<String, Object> globalProperties = new HashMap<String, Object>();

    /**
     * Storage for all configuration objects.
     * @since r4
     */
    private Map<Class<? extends ExtValModuleConfiguration>, ExtValModuleConfiguration> extValConfig =
            new ConcurrentHashMap<Class<? extends ExtValModuleConfiguration>, ExtValModuleConfiguration>();

    /**
     * Configured Module Configuration resolver.
     * @since r4
     */
    private ExtValModuleConfigurationResolver defaultModuleConfigurationResolver;

    private Map<StaticConfigurationNames, List<StaticConfiguration<String, String>>> staticConfigMap
            = new HashMap<StaticConfigurationNames, List<StaticConfiguration<String, String>>>();

    private ExtValContextInternals contextHelper;
    private ExtValContextInvocationOrderAwareInternals invocationOrderAwareContextHelper;

    protected ExtValContext()
    {
        this.contextHelper = new ExtValContextInternals();
        this.invocationOrderAwareContextHelper = new ExtValContextInvocationOrderAwareInternals(this.contextHelper);

        retrieveModuleConfigurationResolver();
    }

    /**
     * Retrieves the Module Configuration Resolver.  Code looks first for a resolver located under the ExtVal Custom
     * package (org.apache.myfaces.extensions.validator.custom.ExtValModuleConfigurationResolver) and then for a
     * web.xml defined initialization parameter org.apache.myfaces.extensions.validator.core.
     * ExtValModuleConfigurationResolver
     * for a fully qualified class name.  The resolver configured by the web.xml overrides the custom defined one.
     */
    private void retrieveModuleConfigurationResolver()
    {
        Object customExtValModuleConfigurationResolver =
                ClassUtils.tryToInstantiateClassForName(CUSTOM_EXTVAL_MODULE_CONFIGURATION_RESOLVER_CLASS_NAME);

        if(customExtValModuleConfigurationResolver instanceof ExtValModuleConfigurationResolver)
        {
            this.defaultModuleConfigurationResolver =
                    (ExtValModuleConfigurationResolver)customExtValModuleConfigurationResolver;
        }

        String customExtValModuleConfigurationResolverClassName =
                getInitParameter(null, ExtValModuleConfigurationResolver.class.getName());

        if(customExtValModuleConfigurationResolverClassName != null)
        {
            customExtValModuleConfigurationResolver =
                    ClassUtils.tryToInstantiateClassForName(customExtValModuleConfigurationResolverClassName);

            if(customExtValModuleConfigurationResolver instanceof ExtValModuleConfigurationResolver)
            {
                this.defaultModuleConfigurationResolver =
                        (ExtValModuleConfigurationResolver)customExtValModuleConfigurationResolver;
            }
        }
    }

    public static ExtValContext getContext()
    {
        if (extValContext == null)
        {
            extValContext = new ExtValContext();

            tryToCreateCustomExtValContext();
        }
        return extValContext;
    }

    private static void tryToCreateCustomExtValContext()
    {
        Object customExtValContext = ClassUtils.tryToInstantiateClassForName(CUSTOM_EXTVAL_CONTEXT_CLASS_NAME);

        if (customExtValContext instanceof ExtValContext)
        {
            extValContext = (ExtValContext) customExtValContext;
        }
    }

    public void setViolationSeverityInterpreter(ViolationSeverityInterpreter violationSeverityInterpreter)
    {
        setViolationSeverityInterpreter(violationSeverityInterpreter, true);
    }

    public void setViolationSeverityInterpreter(
            ViolationSeverityInterpreter violationSeverityInterpreter, boolean forceOverride)
    {
        if (this.violationSeverityInterpreter == null || forceOverride)
        {
            if (violationSeverityInterpreter != null)
            {
                this.logger.info(violationSeverityInterpreter.getClass() + " is used");
            }
            this.violationSeverityInterpreter = violationSeverityInterpreter;
        }
    }

    public ViolationSeverityInterpreter getViolationSeverityInterpreter()
    {
        ViolationSeverityInterpreter requestScopedInterpreter = this.contextHelper
                .getRequestScopedViolationSeverityInterpreter();

        if(requestScopedInterpreter != null)
        {
            return requestScopedInterpreter;
        }

        return this.violationSeverityInterpreter;
    }

    /*
    * FactoryFinder
    */
    public FactoryFinder getFactoryFinder()
    {
        return this.factoryFinder;
    }

    public void setFactoryFinder(FactoryFinder factoryFinder)
    {
        if (factoryFinder != null)
        {
            this.factoryFinder = factoryFinder;
        }
    }

    /*
     * SkipValidationEvaluator
     */
    public void setSkipValidationEvaluator(SkipValidationEvaluator skipValidationEvaluator)
    {
        setSkipValidationEvaluator(skipValidationEvaluator, true);
    }

    public void setSkipValidationEvaluator(SkipValidationEvaluator skipValidationEvaluator, boolean forceOverride)
    {
        if (this.skipValidationEvaluator == null || forceOverride)
        {
            if (skipValidationEvaluator != null)
            {
                this.logger.info(skipValidationEvaluator.getClass() + " is used");
            }
            this.skipValidationEvaluator = skipValidationEvaluator;
        }
    }

    public SkipValidationEvaluator getSkipValidationEvaluator()
    {
        if (this.skipValidationEvaluator == null)
        {
            return new SkipValidationEvaluator()
            {
                public boolean skipValidation(FacesContext facesContext, UIComponent uiComponent,
                                              ValidationStrategy validationStrategy, MetaDataEntry entry)
                {
                    return false;
                }
            };
        }

        return this.skipValidationEvaluator;
    }

    /*
     * RendererInterceptors
     */
    public List<RendererInterceptor> getRendererInterceptors()
    {
        return new ArrayList<RendererInterceptor>(this.rendererInterceptors.values());
    }

    public boolean registerRendererInterceptor(RendererInterceptor rendererInterceptor)
    {
        synchronized (ExtValContext.class)
        {
            if (this.deniedInterceptors.contains(rendererInterceptor.getInterceptorId()))
            {
                return false;
            }

            this.rendererInterceptors.put(rendererInterceptor.getInterceptorId(), rendererInterceptor);
        }
        return true;
    }

    public void deregisterRendererInterceptor(Class<? extends RendererInterceptor> rendererInterceptorClass)
    {
        RendererInterceptor rendererInterceptor = ClassUtils.tryToInstantiateClass(rendererInterceptorClass);

        synchronized (ExtValContext.class)
        {
            this.rendererInterceptors.remove(rendererInterceptor.getInterceptorId());
        }
    }

    //if an interceptor hasn't been registered so far, it should be denied at future registrations
    public void denyRendererInterceptor(Class<? extends RendererInterceptor> rendererInterceptorClass)
    {
        RendererInterceptor rendererInterceptor = ClassUtils.tryToInstantiateClass(rendererInterceptorClass);

        synchronized (ExtValContext.class)
        {
            if (!this.deniedInterceptors.contains(rendererInterceptor.getInterceptorId()))
            {
                this.deniedInterceptors.add(rendererInterceptor.getInterceptorId());
            }
        }
        deregisterRendererInterceptor(rendererInterceptorClass);
    }

    /*
     * ComponentInitializers
     */
    public void addComponentInitializer(ComponentInitializer componentInitializer)
    {
        this.invocationOrderAwareContextHelper.lazyInitComponentInitializers();
        this.invocationOrderAwareContextHelper.addComponentInitializer(componentInitializer);
    }

    public List<ComponentInitializer> getComponentInitializers()
    {
        this.invocationOrderAwareContextHelper.lazyInitComponentInitializers();
        return this.invocationOrderAwareContextHelper.getComponentInitializers();
    }

    /*
     * ValidationExceptionInterceptors
     */
    public void addValidationExceptionInterceptor(ValidationExceptionInterceptor validationExceptionInterceptor)
    {
        this.invocationOrderAwareContextHelper.lazyInitValidationExceptionInterceptors();
        this.invocationOrderAwareContextHelper.addValidationExceptionInterceptor(validationExceptionInterceptor);
    }

    public List<ValidationExceptionInterceptor> getValidationExceptionInterceptors()
    {
        this.invocationOrderAwareContextHelper.lazyInitValidationExceptionInterceptors();
        return this.invocationOrderAwareContextHelper.getValidationExceptionInterceptors();
    }

    /*
     * PropertyValidationInterceptors
     */
    public void addPropertyValidationInterceptor(PropertyValidationInterceptor propertyValidationInterceptor)
    {
        this.invocationOrderAwareContextHelper.lazyInitPropertyValidationInterceptors();
        this.invocationOrderAwareContextHelper.addPropertyValidationInterceptor(propertyValidationInterceptor);
    }

    /**
     * @return all global validation interceptors
     */
    public List<PropertyValidationInterceptor> getPropertyValidationInterceptors()
    {
        this.invocationOrderAwareContextHelper.lazyInitPropertyValidationInterceptors();
        return this.invocationOrderAwareContextHelper.getPropertyValidationInterceptors();
    }

    public List<PropertyValidationInterceptor> getPropertyValidationInterceptorsFor(Class moduleKey)
    {
        this.invocationOrderAwareContextHelper.lazyInitPropertyValidationInterceptors();
        return this.invocationOrderAwareContextHelper.getPropertyValidationInterceptorsFor(moduleKey);
    }

    /*
     * MetaDataExtractionInterceptors
     */
    public void addMetaDataExtractionInterceptor(MetaDataExtractionInterceptor metaDataExtractionInterceptor)
    {
        this.invocationOrderAwareContextHelper.lazyInitMetaDataExtractionInterceptors();
        this.invocationOrderAwareContextHelper.addMetaDataExtractionInterceptor(metaDataExtractionInterceptor);
    }

    /**
     * @return all global meta-data extraction interceptors
     */
    public List<MetaDataExtractionInterceptor> getMetaDataExtractionInterceptors()
    {
        this.invocationOrderAwareContextHelper.lazyInitMetaDataExtractionInterceptors();
        return this.invocationOrderAwareContextHelper.getMetaDataExtractionInterceptors();
    }

    public List<MetaDataExtractionInterceptor> getMetaDataExtractionInterceptorsFor(Class moduleKey)
    {
        Map<String, Object> properties = new HashMap<String, Object>();

        if(moduleKey != null)
        {
            properties.put(ValidationModuleKey.class.getName(), moduleKey);
        }
        return getMetaDataExtractionInterceptorsWith(properties);
    }

    public List<MetaDataExtractionInterceptor> getMetaDataExtractionInterceptorsWith(Map<String, Object> properties)
    {
        this.invocationOrderAwareContextHelper.lazyInitMetaDataExtractionInterceptors();
        return this.invocationOrderAwareContextHelper.getMetaDataExtractionInterceptorsWith(properties);
    }

    /*
     * ProcessedInformationRecorders
     */
    public List<ProcessedInformationRecorder> getProcessedInformationRecorders()
    {
        return this.processedInformationRecorders;
    }

    public void addProcessedInformationRecorder(ProcessedInformationRecorder processedInformationRecorder)
    {
        this.processedInformationRecorders.add(processedInformationRecorder);
    }

    /*
     * InformationProviderBean
     */
    public InformationProviderBean getInformationProviderBean()
    {
        return this.contextHelper.getInformationProviderBean();
    }

    /*
     * StaticConfiguration
     */
    public List<StaticConfiguration<String, String>> getStaticConfiguration(StaticConfigurationNames name)
    {
        if (!this.staticConfigMap.containsKey(name))
        {
            List<StaticConfiguration<String, String>> staticConfigList =
                    new ArrayList<StaticConfiguration<String, String>>();
            this.staticConfigMap.put(name, staticConfigList);
        }
        return this.staticConfigMap.get(name);
    }

    public void addStaticConfiguration(StaticConfigurationNames name, StaticConfiguration<String, String> staticConfig)
    {
        synchronized (this)
        {
            List<StaticConfiguration<String, String>> staticConfigList;
            if (!this.staticConfigMap.containsKey(name))
            {
                staticConfigList = new ArrayList<StaticConfiguration<String, String>>();
                this.staticConfigMap.put(name, staticConfigList);
            }
            this.staticConfigMap.get(name).add(staticConfig);
        }
    }

    /*
     * Global properties
     */
    public boolean addGlobalProperty(String name, Object value)
    {
        return addGlobalProperty(name, value, true);
    }

    public boolean addGlobalProperty(String name, Object value, boolean forceOverride)
    {
        if (this.globalProperties.containsKey(name))
        {
            if (!forceOverride)
            {
                return false;
            }

            this.logger.info("override global property '" + name + "'");
        }

        if(JsfProjectStage.is(JsfProjectStage.Development))
        {
            this.logger.info("global property [" + name + "] added");
        }

        this.globalProperties.put(name, value);
        return true;
    }

    public Object getGlobalProperty(String name)
    {
        return this.globalProperties.get(name);
    }

    /**
     * Retrieves the configuration object of the type specified by the parameter 'targetType'.  The type should be
     * direct descendants of ExtValModuleConfiguration and the same as the type you used to register the configuration
     * (see addModuleConfiguration).
     *
     * @param targetType Key that specifies the type of configuration, should be direct descendants of
     * ExtValModuleConfiguration
     * @param <T>
     * @return  configuration object
     * @since r4
     */
    public <T extends ExtValModuleConfiguration> T getModuleConfiguration(Class<T> targetType)
    {
        ExtValModuleConfiguration result = this.extValConfig.get(targetType);

        //noinspection unchecked
        return (T)result;
    }

    /**
     * Registers the configuration object specified in the parameter 'config' for the type 'key' within ExtVal
     * overriding
     * possible another registration.
     *
     * @param key Key that specifies the type of configuration, should be direct descendants of
     * ExtValModuleConfiguration
     * @param extValConfig Configuration object to register
     * @return true
     * @since r4
     */
    public boolean addModuleConfiguration(Class<? extends ExtValModuleConfiguration> key,
                                          ExtValModuleConfiguration extValConfig)
    {
        return addModuleConfiguration(key, extValConfig, true);
    }

    /**
     * Registers the configuration object specified in the parameter 'config' for the type 'key' within ExtVal.  When a
     * configuration object already exist for the key and we don't specify to override it (parameter forceOverride) the
     * configuration isn't registered.
     * @param key Key that specifies the type of configuration, should be direct descendants of
     * ExtValModuleConfiguration
     * @param config Configuration object to register
     * @param forceOverride Do we override another custom configuration that is already registered.
     * @return true is the configuration is registered or false if already existed and no ofrce override specified.
     * @since r4
     */
    public boolean addModuleConfiguration(Class<? extends ExtValModuleConfiguration> key,
                                          ExtValModuleConfiguration config,
                                          boolean forceOverride)
    {
        // Check if it already exists.
        if (this.extValConfig.containsKey(key))
        {
            if (!forceOverride)
            {
                // Don't forced, so not registered
                return false;
            }

            this.logger.info("override config for key '" + config.getClass() + "'");
        }

        //anonymous class are only supported for test-cases and
        //there we don't need a custom config defined in the web.xml
        // or from a resolver
        if(!config.getClass().isAnonymousClass())
        {
            config = tryToLoadCustomConfigImplementation(config);
        }

        // Store the configuration
        this.extValConfig.put(key, config);

        // Logging when in Development Stage.
        if(JsfProjectStage.is(JsfProjectStage.Development))
        {
            this.logger.info("config for key [" + config.getClass() + "] added");
        }

        return true;
    }

    /**
     * Tries to load a custom configuration implementation by looking for a web.xml initialization parameter. The method
     * returns the configuration object that should be used. That is, the custom defined version or the specified
     * as parameter of the method.
     *
     * @param config The configuration object for which we try to load another version
     * @return The custom configuration object or parameter specified if no alternative found.
     * @since r4
     */
    private ExtValModuleConfiguration tryToLoadCustomConfigImplementation(ExtValModuleConfiguration config)
    {
        // Get the parent if the parameter.  For the default version, like DefaultExtValCoreConfiguration, it is
        // the abstract base class.
        Class configClass = config.getClass().getSuperclass();

        // To be on the safe side that the parent still belongs to the configuration part of ExtVal.
        if(!ExtValModuleConfiguration.class.isAssignableFrom(configClass))
        {
            return config;
        }

        @SuppressWarnings({"unchecked"})
        Class<? extends ExtValModuleConfiguration> configDefinitionClass =
                (Class<? extends  ExtValModuleConfiguration>)configClass;

        // If we have a resolver, use it to retrieve the configuration
        if(this.defaultModuleConfigurationResolver != null)
        {
            config = this.defaultModuleConfigurationResolver.getCustomConfiguration(configDefinitionClass);
        }

        // Lookup the web.xml initialization parameter
        String customConfigClassName = getInitParameter(null, configDefinitionClass.getName());

        if(customConfigClassName != null)
        {
            // If specified, see if it exists and can be used.
            Object customConfig = ClassUtils.tryToInstantiateClassForName(customConfigClassName);

            if(customConfig instanceof ExtValModuleConfiguration)
            {
                return (ExtValModuleConfiguration)customConfig;
            }
        }
        // return the parameter or resolver one.
        return config;
    }
}
