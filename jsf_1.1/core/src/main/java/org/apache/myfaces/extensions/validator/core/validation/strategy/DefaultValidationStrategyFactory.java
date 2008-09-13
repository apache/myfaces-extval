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
package org.apache.myfaces.extensions.validator.core.validation.strategy;

import org.apache.myfaces.extensions.validator.core.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.mapper
    .AnnotationToValidationStrategyBeanNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.mapper
    .CustomConfiguredAnnotationToValidationStrategyNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.mapper
    .CustomConventionAnnotationToValidationStrategyNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.mapper
    .DefaultAnnotationToValidationStrategyNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.mapper
    .SimpleAnnotationToValidationStrategyNameMapper;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.util.ELUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;


/**
 * Factory which creates the ValidationStrategy for a given annotation
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@ToDo(value = Priority.MEDIUM, description = "add generic java api (de-/register mapping)")
@UsageInformation({UsageCategory.INTERNAL, UsageCategory.CUSTOMIZABLE})
public class DefaultValidationStrategyFactory implements
    ClassMappingFactory<Annotation, ValidationStrategy>
{
    private static Map<String, String> annotationStrategyMapping = null;
    private static List<NameMapper<Annotation>> nameMapperList = new ArrayList<NameMapper<Annotation>>();

    static
    {
        nameMapperList
            .add(new CustomConfiguredAnnotationToValidationStrategyNameMapper());
        nameMapperList
            .add(new CustomConventionAnnotationToValidationStrategyNameMapper());
        nameMapperList
            .add(new DefaultAnnotationToValidationStrategyNameMapper());
        nameMapperList
            .add(new SimpleAnnotationToValidationStrategyNameMapper());
        //TODO if jsr 303 doesn't change:
        //nameMapperList.add(new BeanValidationAnnotationStrategyNameMapper());
        nameMapperList
            .add(new AnnotationToValidationStrategyBeanNameMapper(
                new CustomConfiguredAnnotationToValidationStrategyNameMapper()));
        nameMapperList
            .add(new AnnotationToValidationStrategyBeanNameMapper(
                new CustomConventionAnnotationToValidationStrategyNameMapper()));
        nameMapperList.add(new AnnotationToValidationStrategyBeanNameMapper(
            new DefaultAnnotationToValidationStrategyNameMapper()));
        nameMapperList.add(new AnnotationToValidationStrategyBeanNameMapper(
            new SimpleAnnotationToValidationStrategyNameMapper()));
    }

    public ValidationStrategy create(Annotation annotation)
    {
        if (annotationStrategyMapping == null)
        {
            initStaticStrategyMappings();
        }

        String annotationName = annotation.annotationType().getName();

        if (annotationStrategyMapping.containsKey(annotationName))
        {
            return (ValidationStrategy) getValidationStrategyInstance(annotationStrategyMapping
                .get(annotationName));
        }

        ValidationStrategy validationStrategy;
        String strategyName;
        //null -> use name mappers
        for (NameMapper<Annotation> nameMapper : nameMapperList)
        {
            strategyName = nameMapper.createName(annotation);

            if (strategyName == null)
            {
                continue;
            }

            validationStrategy = getValidationStrategyInstance(strategyName);

            if (validationStrategy != null)
            {
                addMapping(annotationName, strategyName);
                return validationStrategy;
            }
        }

        return null;
    }

    private ValidationStrategy getValidationStrategyInstance(
        String validationStrategyName)
    {
        if (validationStrategyName
            .startsWith(AnnotationToValidationStrategyBeanNameMapper.PREFIX_FOR_BEAN_MAPPING))
        {
            return (ValidationStrategy) ELUtils
                .getBean(validationStrategyName
                    .substring(AnnotationToValidationStrategyBeanNameMapper.PREFIX_FOR_BEAN_MAPPING
                    .length()));
        }
        else
        {
            return (ValidationStrategy) ClassUtils
                .tryToInstantiateClassForName(validationStrategyName);
        }
    }

    @ToDo(value = Priority.MEDIUM, description = "logging")
    private void addMapping(String annotationName, String strategyName)
    {
        synchronized (DefaultValidationStrategyFactory.class)
        {
            annotationStrategyMapping.put(annotationName, strategyName);
        }
    }

    @ToDo(value = Priority.MEDIUM, description = "logging")
    private void initStaticStrategyMappings()
    {
        synchronized (DefaultValidationStrategyFactory.class)
        {
            annotationStrategyMapping = new HashMap<String, String>();

            //setup internal static mappings
            for (String internalMappingSource : ExtValUtils
                .getInformationProviderBean()
                .getStaticStrategyMappingSources())
            {
                setupStrategyMappings(internalMappingSource);
            }

            //try to setup mapping with base name by convention - overrides default mapping
            try
            {
                //build convention (strategy mapping)
                setupStrategyMappings(ExtValUtils.getInformationProviderBean()
                    .getCustomStaticStrategyMappingSource());
            }
            catch (Throwable t)
            {
                //do nothing - it was just a try
            }

            //setup custom mapping - overrides all other mappings
            String customMappingBaseName = WebXmlParameter.CUSTOM_VALIDATIONSTRATEGY_MAPPING;
            if (customMappingBaseName != null)
            {
                try
                {
                    setupStrategyMappings(customMappingBaseName);
                }
                catch (MissingResourceException e)
                {
                    e.printStackTrace();
                }
            }
        }
    }

    private void setupStrategyMappings(String bundle)
    {
        ResourceBundle strategyMapping = ResourceBundle.getBundle(bundle);

        if (strategyMapping == null)
        {
            return;
        }

        Enumeration keys = strategyMapping.getKeys();

        String annotationClassName;
        String validationStrategyClassName;

        while (keys.hasMoreElements())
        {
            annotationClassName = (String) keys.nextElement();
            validationStrategyClassName = strategyMapping
                .getString(annotationClassName);

            addMapping(annotationClassName, validationStrategyClassName);
        }
    }
}
