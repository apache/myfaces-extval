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
import org.apache.myfaces.extensions.validator.core.storage.PropertyStorage;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;

/**
 * Contains helper methods that are dealing with annotation usage in the context of ExtVal.
 *
 * @author Gerhard Petracek
 * @since r4
 */
@UsageInformation(UsageCategory.REUSE)
@SuppressWarnings("unchecked")
public class ExtValAnnotationUtils
{
    private static final Logger LOGGER = Logger.getLogger(ExtValAnnotationUtils.class.getName());

    @ToDo(value = Priority.HIGH, description = "add cache")
    /**
     * Extract all annotations found on a property.  It looks for them on getter method, the field and all getters
     * that are defined in interfaces. The name of the property for which the annotations are searched, is passed
     * as value of the field property of the parameter propertyDetails.
     *
     * @param entityClass Class object where the annotations are searched.
     * @param propertyDetails Information on the property where are interested in.
     * @return Object with information and metaDataEntries for the annotations.
     */
    public static PropertyInformation extractAnnotations(Class entityClass, PropertyDetails propertyDetails)
    {
        PropertyInformation propertyInformation = new DefaultPropertyInformation();
        propertyInformation.setInformation(PropertyInformationKeys.PROPERTY_DETAILS, propertyDetails);

        PropertyStorage storage = ReflectionUtils.getPropertyStorage();

        while (!Object.class.getName().equals(entityClass.getName()))
        {
            addPropertyAccessAnnotations(storage, entityClass, propertyDetails.getProperty(), propertyInformation);
            addFieldAccessAnnotations(storage, entityClass, propertyDetails.getProperty(), propertyInformation);

            processInterfaces(storage, entityClass, propertyDetails, propertyInformation);

            entityClass = entityClass.getSuperclass();
        }

        return propertyInformation;
    }

    @ToDo(value = Priority.HIGH, description = "add cache")
    /**
     * Extract all annotations found on the getter method of a property. The annotations are added to the
     * propertyInformation MetaDataEntries.
     *
     * @param storage PropertyStorage where the getter method could be cached.
     * @param entity Class object where the annotations are searched.
     * @param property Name of the property we are interested.
     * @param propertyInformation Where the MetaDataEntries for the annotations are added.
     */
    public static void addPropertyAccessAnnotations(PropertyStorage storage, Class entity,
                                                    String property,
                                                    PropertyInformation propertyInformation)
    {
        Method method = ReflectionUtils.tryToGetMethodOfProperty(storage, entity, property);

        if(method != null)
        {
            addAnnotationToAnnotationEntries(Arrays.asList(method.getAnnotations()), propertyInformation);
        }
    }

    @ToDo(value = Priority.HIGH, description = "add cache")
    /**
     * Extract all annotations found the field of the property. A field name with an additional _ (underscore) is also
     * searched. The annotations are added to the propertyInformation MetaDataEntries.
     * @param storage PropertyStorage where the field could be cached.
     * @param entity Class object where the annotations are searched.
     * @param property Name of the property we are interested.
     * @param propertyInformation Where the MetaDataEntries for the annotations are added.
     */
    public static void addFieldAccessAnnotations(PropertyStorage storage, Class entity,
                                                 String property,
                                                 PropertyInformation propertyInformation)
    {
        Field field = ReflectionUtils.tryToGetFieldOfProperty(storage, entity, property);

        if(field != null)
        {
            addAnnotationToAnnotationEntries(Arrays.asList(field.getAnnotations()), propertyInformation);
        }
    }

    private static void processInterfaces(PropertyStorage storage, Class currentClass,
                                          PropertyDetails propertyDetails,
                                          PropertyInformation propertyInformation)
    {
        for (Class currentInterface : currentClass.getInterfaces())
        {
            addPropertyAccessAnnotations(storage, currentInterface, propertyDetails.getProperty(), propertyInformation);

            processInterfaces(storage, currentInterface, propertyDetails, propertyInformation);
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

    /**
     * Extract the value of the annotation property named value. The value is cast as a value of the parameter
     * targetClass.
     *
     * @param annotation The annotations that is used to extract the property value from.
     * @param targetClass Type of the property named value.
     * @param <T> Result class
     * @return value of the annotation property named value
     */
    public static <T> T extractValueOf(Annotation annotation, Class<T> targetClass)
    {
        // Since we can't be sure of the type, we can't use the
        // annotation.annotationType().getDeclaredMethod(String, Class...) method.
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
