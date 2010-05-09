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
package org.apache.myfaces.extensions.validator.util;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;

import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.DefaultPropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;

/**
 * @author Gerhard Petracek
 * @since r4
 */
@UsageInformation(UsageCategory.REUSE)
@SuppressWarnings("unchecked")
public class ExtValAnnotationUtils
{
    private static final Logger LOGGER = Logger.getLogger(ExtValAnnotationUtils.class.getName());

    @ToDo(value = Priority.HIGH, description = "add cache")
    public static PropertyInformation extractAnnotations(Class entityClass, PropertyDetails propertyDetails)
    {
        PropertyInformation propertyInformation = new DefaultPropertyInformation();
        propertyInformation.setInformation(PropertyInformationKeys.PROPERTY_DETAILS, propertyDetails);

        while (!Object.class.getName().equals(entityClass.getName()))
        {
            addPropertyAccessAnnotations(entityClass, propertyDetails.getProperty(), propertyInformation);
            addFieldAccessAnnotations(entityClass, propertyDetails.getProperty(), propertyInformation);

            processInterfaces(entityClass, propertyDetails, propertyInformation);

            entityClass = entityClass.getSuperclass();
        }

        return propertyInformation;
    }

    @ToDo(value = Priority.HIGH, description = "add cache")
    public static void addPropertyAccessAnnotations(Class entity,
                                                    String property,
                                                    PropertyInformation propertyInformation)
    {
        Method method = ReflectionUtils.tryToGetMethodOfProperty(entity, property);

        if(method != null)
        {
            addAnnotationToAnnotationEntries(Arrays.asList(method.getAnnotations()), propertyInformation);
        }
    }

    @ToDo(value = Priority.HIGH, description = "add cache")
    public static void addFieldAccessAnnotations(Class entity,
                                                 String property,
                                                 PropertyInformation propertyInformation)
    {
        Field field = ReflectionUtils.tryToGetFieldOfProperty(entity, property);

        if(field != null)
        {
            addAnnotationToAnnotationEntries(Arrays.asList(field.getAnnotations()), propertyInformation);
        }
    }

    private static void processInterfaces(Class currentClass,
                                          PropertyDetails propertyDetails,
                                          PropertyInformation propertyInformation)
    {
        for (Class currentInterface : currentClass.getInterfaces())
        {
            addPropertyAccessAnnotations(currentInterface, propertyDetails.getProperty(), propertyInformation);

            processInterfaces(currentInterface, propertyDetails, propertyInformation);
        }
    }

    private static void addAnnotationToAnnotationEntries(List<Annotation> annotations,
                                                         PropertyInformation propertyInformation)
    {
        for (Annotation annotation : annotations)
        {
            propertyInformation.addMetaDataEntry(createMetaDataEntryForAnnotation(annotation));

            LOGGER.finest(annotation.getClass().getName() + " found");
        }
    }

    private static MetaDataEntry createMetaDataEntryForAnnotation(Annotation foundAnnotation)
    {
        MetaDataEntry entry = new MetaDataEntry();

        entry.setKey(foundAnnotation.annotationType().getName());
        entry.setValue(foundAnnotation);

        return entry;
    }

    public static <T> T extractValueOf(Annotation annotation, Class<T> targetClass)
    {
        for (Method annotationMethod : annotation.annotationType().getDeclaredMethods())
        {
            if ("value".equals(annotationMethod.getName()))
            {
                try
                {
                    return (T) annotationMethod.invoke(annotation);
                }
                catch (Exception e)
                {
                    //do nothing
                }
            }
        }
        return null;
    }
}
