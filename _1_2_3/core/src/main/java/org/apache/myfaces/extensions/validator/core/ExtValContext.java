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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
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

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public class ExtValContext
{
    private final Log logger = LogFactory.getLog(getClass());

    private static ExtValContext extValContext;

    //don't try to resolve it dynamically e.g. via InformationProviderBean - there's a mojarra issue
    private static final String CUSTOM_EXTVAL_CONTEXT_CLASS_NAME =
            ExtValContext.class.getName().replace(".core.", ".custom.");

    private ViolationSeverityInterpreter violationSeverityInterpreter;
    private FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
    private Map<String, RendererInterceptor> rendererInterceptors = new HashMap<String, RendererInterceptor>();
    private List<String> deniedInterceptors = new ArrayList<String>();
    private List<ProcessedInformationRecorder> processedInformationRecorders =
            new ArrayList<ProcessedInformationRecorder>();

    private SkipValidationEvaluator skipValidationEvaluator;

    private Map<String, Object> globalProperties = new HashMap<String, Object>();

    private Map<StaticConfigurationNames, List<StaticConfiguration<String, String>>> staticConfigMap
            = new HashMap<StaticConfigurationNames, List<StaticConfiguration<String, String>>>();

    private ExtValContextInternals contextHelper;
    private ExtValContextInvocationOrderAwareInternals invocationOrderAwareContextHelper;

    protected ExtValContext()
    {
        this.contextHelper = new ExtValContextInternals();
        this.invocationOrderAwareContextHelper = new ExtValContextInvocationOrderAwareInternals(this.contextHelper);
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
            if (this.logger.isInfoEnabled() && violationSeverityInterpreter != null)
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
            if (this.logger.isInfoEnabled() && skipValidationEvaluator != null)
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

            if (this.logger.isInfoEnabled())
            {
                this.logger.info("override global property '" + name + "'");
            }
        }

        if(JsfProjectStage.is(JsfProjectStage.Development) && this.logger.isInfoEnabled())
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
}
