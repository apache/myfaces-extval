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
import java.beans.Introspector;
import java.lang.reflect.Method;

import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.storage.MappedConstraintSourceStorage;
import org.apache.myfaces.extensions.validator.core.storage.PropertyStorage;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

/**
 * Contains helper methods that deal with the mapped constraint source mechanism.
 *
 * @author Gerhard Petracek
 * @since r4
 */
@UsageInformation(UsageCategory.INTERNAL)
@SuppressWarnings("unchecked")
public final class ConstraintSourceUtils
{
    protected ConstraintSourceUtils()
    {
        // Utility class, don't allow instantiation.
    }

    public static PropertyDetails resolveMappedConstraintSourceFor(String originalKey,
                                                                   Class originalClass,
                                                                   String originalProperty)
    {
        MappedConstraintSourceStorage mappedConstraintSourceStorage = getConstraintSourceStorage();

        if (isMappedConstraintSourceCached(mappedConstraintSourceStorage, originalClass, originalProperty))
        {
            return getMappedConstraintSource(mappedConstraintSourceStorage, originalClass, originalProperty);
        }

        originalClass = ProxyUtils.getUnproxiedClass(originalClass);

        PropertyStorage propertyStorage = ReflectionUtils.getPropertyStorage();
        Class newClass = findMappedClass(propertyStorage, originalClass, originalProperty);

        //mapped source is ignored via @IgnoreConstraintSource or there is just no mapping annotation at the target
        if (newClass == null)
        {
            tryToCacheMappedConstraintSourceMetaData(
                    mappedConstraintSourceStorage, originalClass, originalProperty, null);

            return null;
        }

        String newProperty = findMappedProperty(propertyStorage, originalClass, newClass, originalProperty);

        PropertyDetails result = new PropertyDetails(originalKey, newClass, newProperty);

        tryToCacheMappedConstraintSourceMetaData(
                mappedConstraintSourceStorage, originalClass, originalProperty, result);

        return result;
    }

    private static boolean isMappedConstraintSourceCached(
            MappedConstraintSourceStorage storage, Class baseBeanClass, String property)
    {
        return storage.containsMapping(baseBeanClass, property);
    }

    private static PropertyDetails getMappedConstraintSource(
            MappedConstraintSourceStorage storage, Class baseBeanClass, String property)
    {
        return storage.getMappedConstraintSource(baseBeanClass, property);
    }

    private static void tryToCacheMappedConstraintSourceMetaData(
            MappedConstraintSourceStorage storage, Class originalClass, String originalProperty, PropertyDetails result)
    {
        storage.storeMapping(originalClass, originalProperty, result);
    }

    private static MappedConstraintSourceStorage getConstraintSourceStorage()
    {
        return ExtValUtils
                .getStorage(MappedConstraintSourceStorage.class, MappedConstraintSourceStorage.class.getName());
    }

    private static Class findMappedClass(PropertyStorage storage, Class baseBeanClass, String property)
    {
        Class<? extends Annotation> constraintSourceAnnotationImplementation = ExtValCoreConfiguration.get()
                .constraintSourceAnnotation();

        Annotation foundConstraintSourceAnnotation = tryToGetAnnotationFromProperty(
                storage, baseBeanClass, property, constraintSourceAnnotationImplementation);

        if (foundConstraintSourceAnnotation == null)
        {
            foundConstraintSourceAnnotation = tryToGetAnnotationFromField(
                    storage, baseBeanClass, property, constraintSourceAnnotationImplementation);
        }

        if (foundConstraintSourceAnnotation == null && !isMappedConstraintSourceIgnored(baseBeanClass, property))
        {
            foundConstraintSourceAnnotation = tryToGetConstraintSourceAnnotationFromClass(
                    baseBeanClass, constraintSourceAnnotationImplementation);
        }

        if (foundConstraintSourceAnnotation != null)
        {
            return ExtValAnnotationUtils.extractValueOf(foundConstraintSourceAnnotation, Class.class);
        }

        return null;
    }

    private static String findMappedProperty(
            PropertyStorage storage, Class baseBeanClass, Class newBaseBeanClass, String originalProperty)
    {
        Annotation targetPropertyAnnotation = getTargetPropertyMetaData(storage, baseBeanClass, originalProperty);
        if (targetPropertyAnnotation != null)
        {
            return extractNewPropertyName(newBaseBeanClass, targetPropertyAnnotation);
        }

        return originalProperty;
    }

    private static Annotation getTargetPropertyMetaData(
            PropertyStorage storage, Class baseBeanClass, String originalProperty)
    {
        Class<? extends Annotation> targetPropertyAnnotation = getTargetPropertyAnnotationImplementation();
        Class<? extends Annotation> targetPropertyIdAnnotation = getTargetPropertyIdAnnotationImplementation();

        Annotation result = findTargetPropertyIdAnnotation(
                storage, baseBeanClass, originalProperty, targetPropertyIdAnnotation);

        if (result == null)
        {
            result = findTargetPropertyAnnotation(
                    storage, baseBeanClass, originalProperty, targetPropertyAnnotation);
        }

        return result;
    }

    private static Class<? extends Annotation> getTargetPropertyAnnotationImplementation()
    {
        return ExtValCoreConfiguration.get().targetPropertyAnnotation();
    }

    private static Class<? extends Annotation> getTargetPropertyIdAnnotationImplementation()
    {
        return ExtValCoreConfiguration.get().targetPropertyIdAnnotation();
    }

    private static String extractNewPropertyName(Class targetClass, Annotation annotation)
    {
        Object annotationValue = ExtValAnnotationUtils.extractValueOf(annotation, Object.class);

        //@TargetProperty
        if (annotationValue instanceof String)
        {
            return (String) annotationValue;
        }

        //@TargetPropertyId
        if (annotationValue instanceof Class)
        {
            return findNameOfAnnotatedProperty(targetClass, (Class) annotationValue);
        }
        return null;
    }

    //EXTVAL-83/use-case 5
    private static String findNameOfAnnotatedProperty(
            Class targetClass, Class<? extends Annotation> customTargetMarkerAnnotation)
    {
        for (Method currentMethod : targetClass.getDeclaredMethods())
        {
            if (currentMethod.isAnnotationPresent(customTargetMarkerAnnotation))
            {
                return convertMethodToPropertyName(currentMethod.getName());
            }
        }

        for (Field currentField : targetClass.getDeclaredFields())
        {
            if (currentField.isAnnotationPresent(customTargetMarkerAnnotation))
            {
                return convertFieldToPropertyName(currentField.getName());
            }
        }
        return null;
    }

    private static String convertMethodToPropertyName(String name)
    {
        String result = name;

        if (name.startsWith("is"))
        {
            result = name.substring(2);
        }
        else if (name.startsWith("get"))
        {
            result = name.substring(3);
        }

        return Introspector.decapitalize(result);
    }

    private static String convertFieldToPropertyName(String name)
    {
        if (name.startsWith("_"))
        {
            return name.substring(1);
        }
        return name;
    }

    private static Annotation findTargetPropertyIdAnnotation(PropertyStorage storage,
                                                             Class baseBeanClass,
                                                             String property,
                                                             Class<? extends Annotation> targetPropertyIdAnnotation)
    {
        Annotation result = tryToGetAnnotationFromProperty(
                storage, baseBeanClass, property, targetPropertyIdAnnotation);

        if (result == null)
        {
            result = tryToGetAnnotationFromField(storage, baseBeanClass, property, targetPropertyIdAnnotation);
        }

        return result;
    }

    private static Annotation findTargetPropertyAnnotation(PropertyStorage storage,
                                                           Class baseBeanClass,
                                                           String property,
                                                           Class<? extends Annotation> targetPropertyAnnotation)
    {
        Annotation result = tryToGetAnnotationFromProperty(storage, baseBeanClass, property, targetPropertyAnnotation);

        if (result == null)
        {
            result = tryToGetAnnotationFromField(storage, baseBeanClass, property, targetPropertyAnnotation);
        }

        return result;
    }

    private static boolean isMappedConstraintSourceIgnored(Class baseBeanClass, String property)
    {
        PropertyStorage storage = ReflectionUtils.getPropertyStorage();
        Method method = ReflectionUtils.tryToGetMethodOfProperty(storage, baseBeanClass, property);

        if (method != null && method.isAnnotationPresent(getIgnoreConstraintSourceAnnotationImplementation()))
        {
            return true;
        }

        Field field = ReflectionUtils.tryToGetFieldOfProperty(storage, baseBeanClass, property);

        if (field != null && field.isAnnotationPresent(getIgnoreConstraintSourceAnnotationImplementation()))
        {
            return true;
        }

        return false;
    }

    private static Annotation tryToGetConstraintSourceAnnotationFromClass(
            Class baseBeanClass, Class<? extends Annotation> annotation)
    {
        if (baseBeanClass.isAnnotationPresent(annotation))
        {
            return baseBeanClass.getAnnotation(annotation);
        }
        return null;
    }

    private static Class<? extends Annotation> getIgnoreConstraintSourceAnnotationImplementation()
    {
        return ExtValCoreConfiguration.get().ignoreConstraintSourceAnnotation();
    }

    private static Annotation tryToGetAnnotationFromField(
            PropertyStorage storage, Class baseBeanClass, String property, Class<? extends Annotation> annotationClass)
    {
        Field field = ReflectionUtils.tryToGetFieldOfProperty(storage, baseBeanClass, property);

        if (field != null && field.isAnnotationPresent(annotationClass))
        {
            return field.getAnnotation(annotationClass);
        }
        return null;
    }

    private static Annotation tryToGetAnnotationFromProperty(PropertyStorage storage,
                                                             Class baseBeanClass,
                                                             String property,
                                                             Class<? extends Annotation> annotationClass)
    {
        Method method = ReflectionUtils.tryToGetMethodOfProperty(storage, baseBeanClass, property);

        if (method != null && method.isAnnotationPresent(annotationClass))
        {
            return method.getAnnotation(annotationClass);
        }
        return null;
    }
}
