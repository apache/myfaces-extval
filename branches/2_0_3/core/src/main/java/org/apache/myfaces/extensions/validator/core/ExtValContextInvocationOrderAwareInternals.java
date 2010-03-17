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
import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.core.interceptor.MetaDataExtractionInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.PropertyValidationInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ClassUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Collections;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
class ExtValContextInvocationOrderAwareInternals
{
    private final Log logger = LogFactory.getLog(getClass());

    private List<MetaDataExtractionInterceptor> metaDataExtractionInterceptors = null;
    private Map<Class, List<MetaDataExtractionInterceptor>> moduleSpecificMetaDataExtractionInterceptors = null;
    private List<ValidationExceptionInterceptor> validationExceptionInterceptors = null;
    private List<PropertyValidationInterceptor> propertyValidationInterceptors = null;
    private Map<Class, List<PropertyValidationInterceptor>> moduleSpecificPropertyValidationInterceptors = null;
    private List<ComponentInitializer> componentInitializers = null;

    private ExtValContextInternals contextHelper;

    ExtValContextInvocationOrderAwareInternals(ExtValContextInternals contextHelper)
    {
        this.contextHelper = contextHelper;
    }

    /*
     * ComponentInitializers
     */
    void addComponentInitializer(ComponentInitializer componentInitializer)
    {
        this.componentInitializers.add(componentInitializer);
        sortComponentInitializers();
    }

    List<ComponentInitializer> getComponentInitializers()
    {
        return this.contextHelper.isComponentInitializationActivated() ?
                this.componentInitializers : new ArrayList<ComponentInitializer>();
    }

    /*
     * ValidationExceptionInterceptors
     */
    void addValidationExceptionInterceptor(ValidationExceptionInterceptor validationExceptionInterceptor)
    {
        this.validationExceptionInterceptors.add(validationExceptionInterceptor);
        sortValidationExceptionInterceptors();
    }

    List<ValidationExceptionInterceptor> getValidationExceptionInterceptors()
    {
        return this.validationExceptionInterceptors;
    }

    /*
     * PropertyValidationInterceptors
     */
    void addPropertyValidationInterceptor(PropertyValidationInterceptor propertyValidationInterceptor)
    {
        if (propertyValidationInterceptor instanceof ValidationModuleAware)
        {
            addPropertyValidationInterceptorForModules(propertyValidationInterceptor);
            sortModuleSpecificPropertyValidationInterceptors();
        }
        else
        {
            addPropertyValidationInterceptorForModule(null, propertyValidationInterceptor);
            sortPropertyValidationInterceptors();
        }
    }

    List<PropertyValidationInterceptor> getPropertyValidationInterceptors()
    {
        return this.propertyValidationInterceptors;
    }

    List<PropertyValidationInterceptor> getPropertyValidationInterceptorsFor(Class moduleKey)
    {
        List<PropertyValidationInterceptor> result = new ArrayList<PropertyValidationInterceptor>();

        result.addAll(getPropertyValidationInterceptors());

        if (moduleKey != null && this.moduleSpecificPropertyValidationInterceptors.containsKey(moduleKey))
        {
            result.addAll(this.moduleSpecificPropertyValidationInterceptors.get(moduleKey));
        }
        return sortPropertyValidationInterceptorList(result);
    }

    private void addPropertyValidationInterceptorForModules(PropertyValidationInterceptor propertyValidationInterceptor)
    {
        Class moduleKey;
        for (String currentModuleKey : ((ValidationModuleAware) propertyValidationInterceptor).getModuleKeys())
        {
            moduleKey = ClassUtils.tryToLoadClassForName(currentModuleKey);

            if (moduleKey == null)
            {
                continue;
            }

            addPropertyValidationInterceptorForModule(moduleKey, propertyValidationInterceptor);
        }
    }

    private void addPropertyValidationInterceptorForModule(
            Class moduleKey, PropertyValidationInterceptor propertyValidationInterceptor)
    {
        if (moduleKey == null)
        {
            this.propertyValidationInterceptors.add(propertyValidationInterceptor);

            if (logger.isTraceEnabled())
            {
                logger.trace(propertyValidationInterceptor.getClass().getName() + " added as global interceptor");
            }
        }
        else
        {
            List<PropertyValidationInterceptor> propertyValidationInterceptorList;
            if (this.moduleSpecificPropertyValidationInterceptors.containsKey(moduleKey))
            {
                propertyValidationInterceptorList = this.moduleSpecificPropertyValidationInterceptors.get(moduleKey);
            }
            else
            {
                propertyValidationInterceptorList = new ArrayList<PropertyValidationInterceptor>();
                this.moduleSpecificPropertyValidationInterceptors.put(moduleKey, propertyValidationInterceptorList);
            }
            propertyValidationInterceptorList.add(propertyValidationInterceptor);

            if (logger.isTraceEnabled())
            {
                logger.trace(propertyValidationInterceptor.getClass().getName() + " added for " + moduleKey.getName());
            }
        }
    }

    /*
     * MetaDataExtractionInterceptors
     */
    void addMetaDataExtractionInterceptor(MetaDataExtractionInterceptor metaDataExtractionInterceptor)
    {
        if(metaDataExtractionInterceptor instanceof ValidationModuleAware)
        {
            addMetaDataExtractionInterceptorForModules(metaDataExtractionInterceptor);
            sortModuleSpecificMetaDataExtractionInterceptors();
        }
        else
        {
            addMetaDataExtractionInterceptorForModule(null, metaDataExtractionInterceptor);
            sortMetaDataExtractionInterceptors();
        }
    }

    private void addMetaDataExtractionInterceptorForModules(MetaDataExtractionInterceptor metaDataExtractionInterceptor)
    {
        Class moduleKey;
        for (String currentModuleKey : ((ValidationModuleAware) metaDataExtractionInterceptor).getModuleKeys())
        {
            moduleKey = ClassUtils.tryToLoadClassForName(currentModuleKey);

            if (moduleKey == null)
            {
                continue;
            }

            addMetaDataExtractionInterceptorForModule(moduleKey, metaDataExtractionInterceptor);
        }
    }

    private void addMetaDataExtractionInterceptorForModule(
            Class moduleKey, MetaDataExtractionInterceptor metaDataExtractionInterceptor)
    {
        if (moduleKey == null)
        {
            this.metaDataExtractionInterceptors.add(metaDataExtractionInterceptor);

            if (logger.isTraceEnabled())
            {
                logger.trace(metaDataExtractionInterceptor.getClass().getName() + " added as global interceptor");
            }
        }
        else
        {
            List<MetaDataExtractionInterceptor> metaDataExtractionInterceptorList;
            if (this.moduleSpecificMetaDataExtractionInterceptors.containsKey(moduleKey))
            {
                metaDataExtractionInterceptorList = this.moduleSpecificMetaDataExtractionInterceptors.get(moduleKey);
            }
            else
            {
                metaDataExtractionInterceptorList = new ArrayList<MetaDataExtractionInterceptor>();
                this.moduleSpecificMetaDataExtractionInterceptors.put(moduleKey, metaDataExtractionInterceptorList);
            }
            metaDataExtractionInterceptorList.add(metaDataExtractionInterceptor);

            if (logger.isTraceEnabled())
            {
                logger.trace(metaDataExtractionInterceptor.getClass().getName() + " added for " + moduleKey.getName());
            }
        }
    }

    List<MetaDataExtractionInterceptor> getMetaDataExtractionInterceptors()
    {
        return this.metaDataExtractionInterceptors;
    }

    List<MetaDataExtractionInterceptor> getMetaDataExtractionInterceptorsWith(Map<String, Object> properties)
    {
        List<MetaDataExtractionInterceptor> result = new ArrayList<MetaDataExtractionInterceptor>();

        result.addAll(getMetaDataExtractionInterceptors());

        Class moduleKey = tryToResolveModuleKey(properties);
        if(moduleKey != null && this.moduleSpecificMetaDataExtractionInterceptors.containsKey(moduleKey))
        {
            result.addAll(this.moduleSpecificMetaDataExtractionInterceptors.get(moduleKey));
        }

        return sortMetaDataExtractionInterceptorList(result);
    }

    private Class tryToResolveModuleKey(Map<String, Object> properties)
    {
        Class moduleKey = null;
        if(properties != null && properties.containsKey(ValidationModuleKey.class.getName()))
        {
            Object foundValue = properties.get(ValidationModuleKey.class.getName());
            if(foundValue instanceof Class)
            {
                moduleKey = (Class)foundValue;
            }
        }
        return moduleKey;
    }

    /*
     * init
     */

    void lazyInitValidationExceptionInterceptors()
    {
        if (validationExceptionInterceptors != null)
        {
            return;
        }

        validationExceptionInterceptors = new ArrayList<ValidationExceptionInterceptor>();
        List<String> validationExceptionInterceptorClassNames = new ArrayList<String>();

        validationExceptionInterceptorClassNames
                .add(WebXmlParameter.CUSTOM_VALIDATION_EXCEPTION_INTERCEPTOR);
        validationExceptionInterceptorClassNames
                .add(this.contextHelper.getInformationProviderBean().get(
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

                if (logger.isTraceEnabled())
                {
                    logger.trace(validationExceptionInterceptor.getClass().getName() + " added");
                }
            }
        }
    }

    void lazyInitMetaDataExtractionInterceptors()
    {
        if (metaDataExtractionInterceptors != null)
        {
            return;
        }

        metaDataExtractionInterceptors = new ArrayList<MetaDataExtractionInterceptor>();
        moduleSpecificMetaDataExtractionInterceptors = new HashMap<Class, List<MetaDataExtractionInterceptor>>();

        List<String> metaDataExtractionInterceptorClassNames = new ArrayList<String>();

        metaDataExtractionInterceptorClassNames
                .add(WebXmlParameter.CUSTOM_META_DATA_EXTRACTION_INTERCEPTOR);
        metaDataExtractionInterceptorClassNames
                .add(this.contextHelper.getInformationProviderBean().get(
                        CustomInformation.META_DATA_EXTRACTION_INTERCEPTOR));

        MetaDataExtractionInterceptor metaDataExtractionInterceptor;
        for (String validationExceptionInterceptorName : metaDataExtractionInterceptorClassNames)
        {
            metaDataExtractionInterceptor =
                    (MetaDataExtractionInterceptor)
                            ClassUtils.tryToInstantiateClassForName(validationExceptionInterceptorName);

            if (metaDataExtractionInterceptor != null)
            {
                addMetaDataExtractionInterceptor(metaDataExtractionInterceptor);
            }
        }
    }

    void lazyInitComponentInitializers()
    {
        if (componentInitializers != null)
        {
            return;
        }

        componentInitializers = new ArrayList<ComponentInitializer>();
        List<String> componentInitializerClassNames = new ArrayList<String>();
        componentInitializerClassNames
                .add(WebXmlParameter.CUSTOM_COMPONENT_INITIALIZER);
        componentInitializerClassNames
                .add(this.contextHelper.getInformationProviderBean().get(CustomInformation.COMPONENT_INITIALIZER));

        ComponentInitializer componentInitializer;
        for (String componentInitializerName : componentInitializerClassNames)
        {
            componentInitializer =
                    (ComponentInitializer) ClassUtils.tryToInstantiateClassForName(componentInitializerName);

            if (componentInitializer != null)
            {
                componentInitializers.add(componentInitializer);

                if (logger.isTraceEnabled())
                {
                    logger.trace(componentInitializer.getClass().getName() + " added");
                }
            }
        }
    }

    void lazyInitPropertyValidationInterceptors()
    {
        if (propertyValidationInterceptors != null)
        {
            return;
        }

        propertyValidationInterceptors = new ArrayList<PropertyValidationInterceptor>();
        moduleSpecificPropertyValidationInterceptors = new HashMap<Class, List<PropertyValidationInterceptor>>();

        List<String> validationInterceptorClassNames = new ArrayList<String>();

        validationInterceptorClassNames
                .add(WebXmlParameter.CUSTOM_PROPERTY_VALIDATION_INTERCEPTOR);
        validationInterceptorClassNames
                .add(this.contextHelper.getInformationProviderBean().get(
                        CustomInformation.PROPERTY_VALIDATION_INTERCEPTOR));

        PropertyValidationInterceptor propertyValidationInterceptor;
        for (String validationInterceptorName : validationInterceptorClassNames)
        {
            propertyValidationInterceptor =
                    (PropertyValidationInterceptor)
                            ClassUtils.tryToInstantiateClassForName(validationInterceptorName);

            if (propertyValidationInterceptor != null)
            {
                if (propertyValidationInterceptor instanceof ValidationModuleAware)
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

    /*
     * sort
     */
    private void sortComponentInitializers()
    {
        Collections.sort(this.componentInitializers, new InvocationOrderComparator<ComponentInitializer>());
    }

    private void sortPropertyValidationInterceptors()
    {
        Collections.sort(this.propertyValidationInterceptors,
                new InvocationOrderComparator<PropertyValidationInterceptor>());
    }

    //sort all - it isn't a huge overhead since it's just done during the init-phase
    private void sortModuleSpecificPropertyValidationInterceptors()
    {
        for(List<PropertyValidationInterceptor> propertyValidationInterceptorList :
                this.moduleSpecificPropertyValidationInterceptors.values())
        {
            sortPropertyValidationInterceptorList(propertyValidationInterceptorList);
        }
    }

    private List<PropertyValidationInterceptor> sortPropertyValidationInterceptorList(
            List<PropertyValidationInterceptor> propertyValidationInterceptorList)
    {
        Collections.sort(propertyValidationInterceptorList,
                    new InvocationOrderComparator<PropertyValidationInterceptor>());

        return propertyValidationInterceptorList;
    }

    private void sortValidationExceptionInterceptors()
    {
        Collections.sort(this.validationExceptionInterceptors,
                new InvocationOrderComparator<ValidationExceptionInterceptor>());
    }

    private void sortMetaDataExtractionInterceptors()
    {
        Collections.sort(this.metaDataExtractionInterceptors,
                new InvocationOrderComparator<MetaDataExtractionInterceptor>());
    }

    //sort all - it isn't a huge overhead since it's just done during the init-phase
    private void sortModuleSpecificMetaDataExtractionInterceptors()
    {
        for(List<MetaDataExtractionInterceptor> metaDataExtractionInterceptorList :
                this.moduleSpecificMetaDataExtractionInterceptors.values())
        {
            sortMetaDataExtractionInterceptorList(metaDataExtractionInterceptorList);
        }
    }

    private List<MetaDataExtractionInterceptor> sortMetaDataExtractionInterceptorList(
            List<MetaDataExtractionInterceptor> metaDataExtractionInterceptorList)
    {
        Collections.sort(metaDataExtractionInterceptorList,
                    new InvocationOrderComparator<MetaDataExtractionInterceptor>());

        return metaDataExtractionInterceptorList;
    }
}