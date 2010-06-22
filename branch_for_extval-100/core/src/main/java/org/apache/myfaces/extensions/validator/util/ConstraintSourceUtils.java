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
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

/**
 * @author Gerhard Petracek
 * @since r4
 */
@UsageInformation(UsageCategory.INTERNAL)
@SuppressWarnings("unchecked")
public final class ConstraintSourceUtils
{
    private ConstraintSourceUtils()
    {
        // Utility class, don't allow instantiation.
    }

    public static PropertyDetails resolveMappedConstraintSourceFor(String originalKey,
                                                                   Class originalClass,
                                                                   String originalProperty)
    {
        if (isMappedConstraintSourceCached(originalClass, originalProperty))
        {
            return getMappedConstraintSource(originalClass, originalProperty);
        }

        originalClass = ProxyUtils.getUnproxiedClass(originalClass);

        Class newClass = findMappedClass(originalClass, originalProperty);

        //mapped source is ignored via @IgnoreConstraintSource or there is just no mapping annotation at the target
        if (newClass == null)
        {
            tryToCacheMappedConstraintSourceMetaData(originalClass, originalProperty, null);
            return null;
        }

        String newProperty = findMappedProperty(originalClass, newClass, originalProperty);

        PropertyDetails result = new PropertyDetails(originalKey, newClass, newProperty);

        tryToCacheMappedConstraintSourceMetaData(originalClass, originalProperty, result);
        return result;
    }

    private static boolean isMappedConstraintSourceCached(Class baseBeanClass, String property)
    {
        return getConstraintSourceStorage().containsMapping(baseBeanClass, property);
    }

    private static PropertyDetails getMappedConstraintSource(Class baseBeanClass, String property)
    {
        return getConstraintSourceStorage().getMappedConstraintSource(baseBeanClass, property);
    }

    private static void tryToCacheMappedConstraintSourceMetaData(
            Class originalClass, String originalProperty, PropertyDetails result)
    {
        getConstraintSourceStorage().storeMapping(originalClass, originalProperty, result);
    }

    private static MappedConstraintSourceStorage getConstraintSourceStorage()
    {
        return ExtValUtils
                .getStorage(MappedConstraintSourceStorage.class, MappedConstraintSourceStorage.class.getName());
    }

    private static Class findMappedClass(Class baseBeanClass, String property)
    {
        Class<? extends Annotation> constraintSourceAnnotationImplementation = ExtValCoreConfiguration.get()
                .constraintSourceAnnotation();

        Annotation foundConstraintSourceAnnotation = tryToGetAnnotationFromProperty(
                baseBeanClass, property, constraintSourceAnnotationImplementation);

        if (foundConstraintSourceAnnotation == null)
        {
            foundConstraintSourceAnnotation = tryToGetAnnotationFromField(
                    baseBeanClass, property, constraintSourceAnnotationImplementation);
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

    private static String findMappedProperty(Class baseBeanClass, Class newBaseBeanClass, String originalProperty)
    {
        Annotation targetPropertyAnnotation = getTargetPropertyMetaData(baseBeanClass, originalProperty);
        if (targetPropertyAnnotation != null)
        {
            return extractNewPropertyName(newBaseBeanClass, targetPropertyAnnotation);
        }

        return originalProperty;
    }

    private static Annotation getTargetPropertyMetaData(Class baseBeanClass, String originalProperty)
    {
        Class<? extends Annotation> targetPropertyAnnotation = getTargetPropertyAnnotationImplementation();
        Class<? extends Annotation> targetPropertyIdAnnotation = getTargetPropertyIdAnnotationImplementation();

        Annotation result = findTargetPropertyIdAnnotation(baseBeanClass, originalProperty, targetPropertyIdAnnotation);

        if (result == null)
        {
            result = findTargetPropertyAnnotation(baseBeanClass, originalProperty, targetPropertyAnnotation);
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

    private static Annotation findTargetPropertyIdAnnotation(Class baseBeanClass,
                                                             String property,
                                                             Class<? extends Annotation> targetPropertyIdAnnotation)
    {
        Annotation result = tryToGetAnnotationFromProperty(baseBeanClass, property, targetPropertyIdAnnotation);

        if (result == null)
        {
            result = tryToGetAnnotationFromField(baseBeanClass, property, targetPropertyIdAnnotation);
        }

        return result;
    }

    private static Annotation findTargetPropertyAnnotation(Class baseBeanClass,
                                                           String property,
                                                           Class<? extends Annotation> targetPropertyAnnotation)
    {
        Annotation result = tryToGetAnnotationFromProperty(baseBeanClass, property, targetPropertyAnnotation);

        if (result == null)
        {
            result = tryToGetAnnotationFromField(baseBeanClass, property, targetPropertyAnnotation);
        }

        return result;
    }

    private static boolean isMappedConstraintSourceIgnored(Class baseBeanClass, String property)
    {
        Method method = ReflectionUtils.tryToGetMethodOfProperty(baseBeanClass, property);

        if (method != null && method.isAnnotationPresent(getIgnoreConstraintSourceAnnotationImplementation()))
        {
            return true;
        }

        Field field = ReflectionUtils.tryToGetFieldOfProperty(baseBeanClass, property);

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
        return (Class) ExtValCoreConfiguration.get().ignoreConstraintSourceAnnotation();
    }

    private static Annotation tryToGetAnnotationFromField(
            Class baseBeanClass, String property, Class<? extends Annotation> annotationClass)
    {
        Field field = ReflectionUtils.tryToGetFieldOfProperty(baseBeanClass, property);

        if (field != null && field.isAnnotationPresent(annotationClass))
        {
            return field.getAnnotation(annotationClass);
        }
        return null;
    }

    private static Annotation tryToGetAnnotationFromProperty(Class baseBeanClass,
                                                             String property,
                                                             Class<? extends Annotation> annotationClass)
    {
        Method method = ReflectionUtils.tryToGetMethodOfProperty(baseBeanClass, property);

        if (method != null && method.isAnnotationPresent(annotationClass))
        {
            return method.getAnnotation(annotationClass);
        }
        return null;
    }
}
