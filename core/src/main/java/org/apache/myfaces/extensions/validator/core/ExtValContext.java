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

import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfiguration;
import org.apache.myfaces.extensions.validator.core.interceptor.RendererInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.MetaDataExtractionInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.PropertyValidationInterceptor;
import org.apache.myfaces.extensions.validator.core.recorder.ProcessedInformationRecorder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationNames;
import org.apache.myfaces.extensions.validator.core.validation.SkipValidationEvaluator;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
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

    private FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
    private Map<String, RendererInterceptor> rendererInterceptors = new HashMap<String, RendererInterceptor>();
    private List<String> deniedInterceptors = new ArrayList<String>();
    private List<ProcessedInformationRecorder> processedInformationRecorders =
        new ArrayList<ProcessedInformationRecorder>();

    private List<ComponentInitializer> componentInitializers;
    private List<ValidationExceptionInterceptor> validationExceptionInterceptors;
    private List<PropertyValidationInterceptor> propertyValidationInterceptors;
    private Map<Class, List<PropertyValidationInterceptor>> moduleSpecificPropertyValidationInterceptors;
    private List<MetaDataExtractionInterceptor> metaDataExtractionInterceptors;

    private SkipValidationEvaluator skipValidationEvaluator;

    private Map<String, Object> globalProperties = new HashMap<String, Object>();

    private Map<StaticConfigurationNames, List<StaticConfiguration<String, String>>> staticConfigMap
        = new HashMap<StaticConfigurationNames, List<StaticConfiguration<String, String>>>();


    private void lazyInitComponentInitializers()
    {
        if(this.componentInitializers != null)
        {
            return;
        }

        this.componentInitializers = new ArrayList<ComponentInitializer>();
        List<String> componentInitializerClassNames = new ArrayList<String>();
        componentInitializerClassNames
            .add(WebXmlParameter.CUSTOM_COMPONENT_INITIALIZER);
        componentInitializerClassNames
            .add(ExtValContext.getContext().getInformationProviderBean().get(CustomInformation.COMPONENT_INITIALIZER));

        ComponentInitializer componentInitializer;
        for (String componentInitializerName : componentInitializerClassNames)
        {
            componentInitializer =
                (ComponentInitializer) ClassUtils.tryToInstantiateClassForName(componentInitializerName);

            if (componentInitializer != null)
            {
                componentInitializers.add(componentInitializer);

                if(logger.isTraceEnabled())
                {
                    logger.trace(componentInitializer.getClass().getName() + " added");
                }
            }
        }
    }

    private void lazyInitValidationExceptionInterceptors()
    {
        if(this.validationExceptionInterceptors != null)
        {
            return;
        }

        this.validationExceptionInterceptors = new ArrayList<ValidationExceptionInterceptor>();
        List<String> validationExceptionInterceptorClassNames = new ArrayList<String>();

        validationExceptionInterceptorClassNames
            .add(WebXmlParameter.CUSTOM_VALIDATION_EXCEPTION_INTERCEPTOR);
        validationExceptionInterceptorClassNames
            .add(ExtValContext.getContext().getInformationProviderBean().get(
                    CustomInformation.VALIDATION_EXCEPTION_INTERCEPTOR));

        ValidationExceptionInterceptor validationExceptionInterceptor;
        for (String validationExceptionInterceptorName : validationExceptionInterceptorClassNames)
        {
            validationExceptionInterceptor =
                (ValidationExceptionInterceptor)
                        ClassUtils.tryToInstantiateClassForName(validationExceptionInterceptorName);

            if (validationExceptionInterceptor != null)
            {
                validationExceptionInterceptors.add(validationExceptionInterceptor);

                if(logger.isTraceEnabled())
                {
                    logger.trace(validationExceptionInterceptor.getClass().getName() + " added");
                }
            }
        }
    }

    private void lazyInitPropertyValidationInterceptors()
    {
        if(this.propertyValidationInterceptors != null)
        {
            return;
        }

        this.propertyValidationInterceptors = new ArrayList<PropertyValidationInterceptor>();
        this.moduleSpecificPropertyValidationInterceptors = new HashMap<Class, List<PropertyValidationInterceptor>>();

        List<String> validationInterceptorClassNames = new ArrayList<String>();

        validationInterceptorClassNames
            .add(WebXmlParameter.CUSTOM_PROPERTY_VALIDATION_INTERCEPTOR);
        validationInterceptorClassNames
            .add(ExtValContext.getContext().getInformationProviderBean().get(
                    CustomInformation.PROPERTY_VALIDATION_INTERCEPTOR));

        PropertyValidationInterceptor propertyValidationInterceptor;
        for (String validationInterceptorName : validationInterceptorClassNames)
        {
            propertyValidationInterceptor =
                (PropertyValidationInterceptor)
                        ClassUtils.tryToInstantiateClassForName(validationInterceptorName);

            if (propertyValidationInterceptor != null)
            {
                if(propertyValidationInterceptor instanceof ValidationModuleAware)
                {
                    addPropertyValidationInterceptorForModules(propertyValidationInterceptor);
                }
                else
                {
                    addPropertyValidationInterceptorForModule(null, propertyValidationInterceptor);
                }
            }
        }
    }

    private void addPropertyValidationInterceptorForModules(PropertyValidationInterceptor propertyValidationInterceptor)
    {
        Class moduleKey;
        for(String currentModuleKey : ((ValidationModuleAware)propertyValidationInterceptor).getModuleKeys())
        {
            moduleKey = ClassUtils.tryToLoadClassForName(currentModuleKey);

            if(moduleKey == null)
            {
                continue;
            }

            addPropertyValidationInterceptorForModule(moduleKey, propertyValidationInterceptor);
        }
    }

    private void addPropertyValidationInterceptorForModule(
            Class moduleKey, PropertyValidationInterceptor propertyValidationInterceptor)
    {
        if(moduleKey == null)
        {
            propertyValidationInterceptors.add(propertyValidationInterceptor);

            if(logger.isTraceEnabled())
            {
                logger.trace(propertyValidationInterceptor.getClass().getName() + " added");
            }
        }
        else
        {
            List<PropertyValidationInterceptor> propertyValidationInterceptorList;
            if(this.moduleSpecificPropertyValidationInterceptors.containsKey(moduleKey))
            {
                propertyValidationInterceptorList = this.moduleSpecificPropertyValidationInterceptors.get(moduleKey);
            }
            else
            {
                propertyValidationInterceptorList = new ArrayList<PropertyValidationInterceptor>();
                this.moduleSpecificPropertyValidationInterceptors.put(moduleKey, propertyValidationInterceptorList);
            }
            propertyValidationInterceptorList.add(propertyValidationInterceptor);

            if(logger.isTraceEnabled())
            {
                logger.trace(propertyValidationInterceptor.getClass().getName() + " added for " + moduleKey.getName());
            }
        }
    }

    private void lazyInitMetaDataExtractionInterceptors()
    {
        if(this.metaDataExtractionInterceptors != null)
        {
            return;
        }

        this.metaDataExtractionInterceptors = new ArrayList<MetaDataExtractionInterceptor>();

        List<String> metaDataExtractionInterceptorClassNames = new ArrayList<String>();

        metaDataExtractionInterceptorClassNames
            .add(WebXmlParameter.CUSTOM_META_DATA_EXTRACTION_INTERCEPTOR);
        metaDataExtractionInterceptorClassNames
            .add(ExtValContext.getContext().getInformationProviderBean().get(
                    CustomInformation.META_DATA_EXTRACTION_INTERCEPTOR));

        MetaDataExtractionInterceptor metaDataExtractionInterceptor;
        for (String validationExceptionInterceptorName : metaDataExtractionInterceptorClassNames)
        {
            metaDataExtractionInterceptor =
                (MetaDataExtractionInterceptor)
                        ClassUtils.tryToInstantiateClassForName(validationExceptionInterceptorName);

            if (metaDataExtractionInterceptor != null)
            {
                this.metaDataExtractionInterceptors.add(metaDataExtractionInterceptor);

                if(logger.isTraceEnabled())
                {
                    logger.trace(metaDataExtractionInterceptor.getClass().getName() + " added");
                }
            }
        }
    }

    public static ExtValContext getContext()
    {
        if(extValContext == null)
        {
            extValContext = new ExtValContext();
            Object customExtValContext = ExtValUtils.getELHelper().getBean(
                    extValContext.getInformationProviderBean().get(CustomInformation.EXTVAL_CONTEXT));

            if(customExtValContext instanceof ExtValContext)
            {
                extValContext = (ExtValContext)customExtValContext;
            }
        }
        return extValContext;
    }

    public FactoryFinder getFactoryFinder()
    {
        return factoryFinder;
    }

    public void setFactoryFinder(FactoryFinder factoryFinder)
    {
        if(factoryFinder != null)
        {
            this.factoryFinder = factoryFinder;
        }
    }

    public void setSkipValidationEvaluator(SkipValidationEvaluator skipValidationEvaluator)
    {
        setSkipValidationEvaluator(skipValidationEvaluator, true);
    }

    public void setSkipValidationEvaluator(SkipValidationEvaluator skipValidationEvaluator, boolean forceOverride)
    {
        if(this.skipValidationEvaluator == null || forceOverride)
        {
            if(this.logger.isInfoEnabled())
            {
                this.logger.info(skipValidationEvaluator != null ?
                        skipValidationEvaluator.getClass() : "no" + " is used");
            }
            this.skipValidationEvaluator = skipValidationEvaluator;
        }
    }

    public SkipValidationEvaluator getSkipValidationEvaluator()
    {
        if(this.skipValidationEvaluator == null)
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

    public List<RendererInterceptor> getRendererInterceptors()
    {
        return new ArrayList<RendererInterceptor>(rendererInterceptors.values());
    }

    public boolean registerRendererInterceptor(RendererInterceptor rendererInterceptor)
    {
        synchronized (ExtValContext.class)
        {
            if (deniedInterceptors.contains(rendererInterceptor.getInterceptorId()))
            {
                return false;
            }

            rendererInterceptors.put(rendererInterceptor.getInterceptorId(), rendererInterceptor);
        }
        return true;
    }

    public void deregisterRendererInterceptor(Class rendererInterceptorClass)
    {
        RendererInterceptor rendererInterceptor =
            (RendererInterceptor) ClassUtils.tryToInstantiateClass(rendererInterceptorClass);

        synchronized (ExtValContext.class)
        {
            rendererInterceptors.remove(rendererInterceptor.getInterceptorId());
        }
    }

    //if an interceptor hasn't been registered so far, it should be denied at future registrations
    public void denyRendererInterceptor(Class rendererInterceptorClass)
    {
        RendererInterceptor rendererInterceptor =
            (RendererInterceptor) ClassUtils.tryToInstantiateClass(rendererInterceptorClass);

        synchronized (ExtValContext.class)
        {
            deniedInterceptors.add(rendererInterceptor.getInterceptorId());
        }
        deregisterRendererInterceptor(rendererInterceptorClass);
    }

    public void addComponentInitializer(ComponentInitializer componentInitializer)
    {
        lazyInitComponentInitializers();
        this.componentInitializers.add(componentInitializer);
    }

    public List<ComponentInitializer> getComponentInitializers()
    {
        lazyInitComponentInitializers();
        return componentInitializers;
    }

    public void addValidationExceptionInterceptor(ValidationExceptionInterceptor validationExceptionInterceptor)
    {
        lazyInitValidationExceptionInterceptors();
        this.validationExceptionInterceptors.add(validationExceptionInterceptor);
    }

    public List<ValidationExceptionInterceptor> getValidationExceptionInterceptors()
    {
        lazyInitValidationExceptionInterceptors();
        return this.validationExceptionInterceptors;
    }

    public void addPropertyValidationInterceptor(PropertyValidationInterceptor propertyValidationInterceptor)
    {
        lazyInitPropertyValidationInterceptors();

        if(propertyValidationInterceptor instanceof ValidationModuleAware)
        {
            addPropertyValidationInterceptorForModules(propertyValidationInterceptor);
        }
        else
        {
            addPropertyValidationInterceptorForModule(null, propertyValidationInterceptor);
        }
    }

    public List<PropertyValidationInterceptor> getPropertyValidationInterceptors()
    {
        lazyInitPropertyValidationInterceptors();
        return this.propertyValidationInterceptors;
    }

    public List<PropertyValidationInterceptor> getPropertyValidationInterceptorsFor(Class moduleKey)
    {
        List<PropertyValidationInterceptor> generalInterceptors = getPropertyValidationInterceptors();

        if(moduleKey == null || !this.moduleSpecificPropertyValidationInterceptors.containsKey(moduleKey))
        {
            return generalInterceptors;
        }

        List<PropertyValidationInterceptor> moduleSpecificInterceptors =
                this.moduleSpecificPropertyValidationInterceptors.get(moduleKey);

        List<PropertyValidationInterceptor> result = new ArrayList<PropertyValidationInterceptor>();
        result.addAll(generalInterceptors);
        result.addAll(moduleSpecificInterceptors);

        return result;
    }

    public void addMetaDataExtractionInterceptor(MetaDataExtractionInterceptor metaDataExtractionInterceptor)
    {
        lazyInitMetaDataExtractionInterceptors();
        metaDataExtractionInterceptors.add(metaDataExtractionInterceptor);
    }

    public List<MetaDataExtractionInterceptor> getMetaDataExtractionInterceptors()
    {
        lazyInitMetaDataExtractionInterceptors();
        return metaDataExtractionInterceptors;
    }

    public List<ProcessedInformationRecorder> getProcessedInformationRecorders()
    {
        return processedInformationRecorders;
    }

    public void addProcessedInformationRecorder(ProcessedInformationRecorder processedInformationRecorder)
    {
        this.processedInformationRecorders.add(processedInformationRecorder);
    }

    public InformationProviderBean getInformationProviderBean()
    {
        Map applicationMap = FacesContext.getCurrentInstance().getExternalContext().getApplicationMap();
        InformationProviderBean bean = (InformationProviderBean) applicationMap.get(InformationProviderBean.BEAN_NAME);

        if (bean == null)
        {
            return initInformationProviderBean(applicationMap);
        }
        return bean;
    }

    @SuppressWarnings({"unchecked"})
    private InformationProviderBean initInformationProviderBean(Map applicationMap)
    {
        List<String> informationProviderBeanClassNames = new ArrayList<String>();

        informationProviderBeanClassNames.add(WebXmlParameter.CUSTOM_INFORMATION_PROVIDER_BEAN);
        informationProviderBeanClassNames.add(InformationProviderBean.CUSTOM_BEAN);

        InformationProviderBean informationProviderBean;
        for (String className : informationProviderBeanClassNames)
        {
            informationProviderBean = (InformationProviderBean) ClassUtils.tryToInstantiateClassForName(className);

            if (informationProviderBean != null)
            {
                applicationMap.put(InformationProviderBean.BEAN_NAME, informationProviderBean);
                return informationProviderBean;
            }
        }

        tryToInitCustomConfiguredInformationProviderBeanClassName(applicationMap);

        if(applicationMap.containsKey(InformationProviderBean.BEAN_NAME))
        {
            return (InformationProviderBean)applicationMap.get(InformationProviderBean.BEAN_NAME);
        }
        return new InformationProviderBean();
    }

    @SuppressWarnings({"unchecked"})
    private void tryToInitCustomConfiguredInformationProviderBeanClassName(Map applicationMap)
    {
        InformationProviderBean bean = (InformationProviderBean) ExtValUtils.getELHelper()
            .getBean(InformationProviderBean.CUSTOM_BEAN.replace(".", "_"));

        if(bean != null)
        {
            applicationMap.put(InformationProviderBean.BEAN_NAME, bean);
        }
    }

    public List<StaticConfiguration<String, String>> getStaticConfiguration(StaticConfigurationNames name)
    {
        if(!this.staticConfigMap.containsKey(name))
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
            if(!this.staticConfigMap.containsKey(name))
            {
                staticConfigList = new ArrayList<StaticConfiguration<String, String>>();
                this.staticConfigMap.put(name, staticConfigList);
            }
            this.staticConfigMap.get(name).add(staticConfig);
        }
    }

    public boolean addGlobalProperty(String name, Object value)
    {
        return addGlobalProperty(name , value, true);
    }

    public boolean addGlobalProperty(String name, Object value, boolean forceOverride)
    {
        if(this.globalProperties.containsKey(name))
        {
            if(!forceOverride)
            {
                return false;
            }

            if(this.logger.isInfoEnabled())
            {
                logger.info("override global property '" + name + "'");
            }
        }

        this.globalProperties.put(name, value);
        return true;
    }

    public Object getGlobalProperty(String name)
    {
        return this.globalProperties.get(name);
    }
}
