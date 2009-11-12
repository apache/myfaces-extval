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
            addPropertyValidationInterceptorForModules(propertyValidationInterceptor,
                    this.propertyValidationInterceptors, this.moduleSpecificPropertyValidationInterceptors);
        }
        else
        {
            addPropertyValidationInterceptorForModule(null, propertyValidationInterceptor,
                    this.propertyValidationInterceptors, this.moduleSpecificPropertyValidationInterceptors);
        }

        sortPropertyValidationInterceptors();
    }

    List<PropertyValidationInterceptor> getPropertyValidationInterceptors()
    {
        return this.propertyValidationInterceptors;
    }

    List<PropertyValidationInterceptor> getPropertyValidationInterceptorsFor(Class moduleKey)
    {
        List<PropertyValidationInterceptor> generalInterceptors = getPropertyValidationInterceptors();

        if (moduleKey == null || !this.moduleSpecificPropertyValidationInterceptors.containsKey(moduleKey))
        {
            return generalInterceptors;
        }

        List<PropertyValidationInterceptor> moduleSpecificInterceptors =
                this.moduleSpecificPropertyValidationInterceptors.get(moduleKey);

        List<PropertyValidationInterceptor> result = new ArrayList<PropertyValidationInterceptor>();
        result.addAll(generalInterceptors);
        result.addAll(moduleSpecificInterceptors);

        sortPropertyValidationInterceptors(result);
        return result;
    }

    private void addPropertyValidationInterceptorForModules(PropertyValidationInterceptor propertyValidationInterceptor,
                                                    List<PropertyValidationInterceptor> propertyValidationInterceptors,
                                                    Map<Class, List<PropertyValidationInterceptor>>
                                                            moduleSpecificPropertyValidationInterceptors)
    {
        Class moduleKey;
        for (String currentModuleKey : ((ValidationModuleAware) propertyValidationInterceptor).getModuleKeys())
        {
            moduleKey = ClassUtils.tryToLoadClassForName(currentModuleKey);

            if (moduleKey == null)
            {
                continue;
            }

            addPropertyValidationInterceptorForModule(moduleKey,
                    propertyValidationInterceptor,
                    propertyValidationInterceptors,
                    moduleSpecificPropertyValidationInterceptors);
        }
    }

    private void addPropertyValidationInterceptorForModule(Class moduleKey,
                                                   PropertyValidationInterceptor propertyValidationInterceptor,
                                                   List<PropertyValidationInterceptor> propertyValidationInterceptors,
                                                   Map<Class, List<PropertyValidationInterceptor>>
                                                           moduleSpecificPropertyValidationInterceptors)
    {
        if (moduleKey == null)
        {
            propertyValidationInterceptors.add(propertyValidationInterceptor);

            if (logger.isTraceEnabled())
            {
                logger.trace(propertyValidationInterceptor.getClass().getName() + " added");
            }
        }
        else
        {
            List<PropertyValidationInterceptor> propertyValidationInterceptorList;
            if (moduleSpecificPropertyValidationInterceptors.containsKey(moduleKey))
            {
                propertyValidationInterceptorList = moduleSpecificPropertyValidationInterceptors.get(moduleKey);
            }
            else
            {
                propertyValidationInterceptorList = new ArrayList<PropertyValidationInterceptor>();
                moduleSpecificPropertyValidationInterceptors.put(moduleKey, propertyValidationInterceptorList);
            }
            propertyValidationInterceptorList.add(propertyValidationInterceptor);

            sortModuleSpecificPropertyValidationInterceptors(propertyValidationInterceptorList);

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
        this.metaDataExtractionInterceptors.add(metaDataExtractionInterceptor);
        sortMetaDataExtractionInterceptors();
    }

    List<MetaDataExtractionInterceptor> getMetaDataExtractionInterceptors()
    {
        return this.metaDataExtractionInterceptors;
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
                metaDataExtractionInterceptors.add(metaDataExtractionInterceptor);

                if (logger.isTraceEnabled())
                {
                    logger.trace(metaDataExtractionInterceptor.getClass().getName() + " added");
                }
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
                    addPropertyValidationInterceptorForModules(propertyValidationInterceptor,
                            propertyValidationInterceptors,
                            moduleSpecificPropertyValidationInterceptors);
                }
                else
                {
                    addPropertyValidationInterceptorForModule(null, propertyValidationInterceptor,
                            propertyValidationInterceptors,
                            moduleSpecificPropertyValidationInterceptors);
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

    private void sortPropertyValidationInterceptors(List<PropertyValidationInterceptor> result)
    {
        Collections.sort(result, new InvocationOrderComparator<PropertyValidationInterceptor>());
    }

    private void sortPropertyValidationInterceptors()
    {
        Collections.sort(this.propertyValidationInterceptors,
                new InvocationOrderComparator<PropertyValidationInterceptor>());
    }

    private void sortValidationExceptionInterceptors()
    {
        Collections.sort(this.validationExceptionInterceptors,
                new InvocationOrderComparator<ValidationExceptionInterceptor>());
    }

    private void sortModuleSpecificPropertyValidationInterceptors(
            List<PropertyValidationInterceptor> propertyValidationInterceptorList)
    {
        if (propertyValidationInterceptorList != null)
        {
            Collections.sort(propertyValidationInterceptorList, new InvocationOrderComparator<Object>());
        }
    }

    private void sortMetaDataExtractionInterceptors()
    {
        Collections.sort(this.metaDataExtractionInterceptors,
                new InvocationOrderComparator<MetaDataExtractionInterceptor>());
    }
}