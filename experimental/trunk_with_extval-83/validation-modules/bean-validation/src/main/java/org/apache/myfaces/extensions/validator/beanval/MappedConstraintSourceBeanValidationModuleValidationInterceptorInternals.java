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
package org.apache.myfaces.extensions.validator.beanval;

import org.apache.commons.logging.Log;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.storage.MappedConstraintSourceStorage;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.validation.ConstraintSource;
import org.apache.myfaces.extensions.validator.core.validation.IgnoreConstraintSource;
import org.apache.myfaces.extensions.validator.core.validation.TargetProperty;
import org.apache.myfaces.extensions.validator.core.validation.TargetPropertyId;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ClassUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.validation.ConstraintViolation;
import javax.validation.ValidatorFactory;
import javax.validation.groups.Default;
import javax.validation.metadata.ElementDescriptor;
import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Set;
import java.util.Collections;

/**
 * @author Gerhard Petracek
 * @since x.x.4
 */
@UsageInformation(UsageCategory.INTERNAL)
class MappedConstraintSourceBeanValidationModuleValidationInterceptorInternals
{
    private Log logger;
    private BeanValidationModuleValidationInterceptorInternals bviUtils;

    MappedConstraintSourceBeanValidationModuleValidationInterceptorInternals(
            Log logger, BeanValidationModuleValidationInterceptorInternals bviUtils)
    {
        this.logger = logger;
        this.bviUtils = bviUtils;
    }

    void initComponentWithPropertyDetailsOfMappedConstraintSource(FacesContext facesContext,
                                                                  UIComponent uiComponent,
                                                                  PropertyDetails propertyDetails)
    {
        Class[] foundGroups = this.bviUtils.resolveGroups(facesContext, uiComponent);

        if (foundGroups == null)
        {
            return;
        }
        else if (foundGroups.length == 0)
        {
            foundGroups = new Class[]{Default.class};
        }

        PropertyDetails constraintSourcePropertyDetails = resolveMappedConstraintSourceFor(
                propertyDetails.getKey(), propertyDetails.getBaseObject().getClass(), propertyDetails.getProperty());

        if(constraintSourcePropertyDetails == null)
        {
            return;
        }

        ElementDescriptor elementDescriptor =
                this.bviUtils.getDescriptorFor((Class) constraintSourcePropertyDetails.getBaseObject(),
                        constraintSourcePropertyDetails.getProperty());

        if (elementDescriptor == null)
        {
            return;
        }

        this.bviUtils.processElementDescriptor(facesContext, uiComponent, foundGroups, elementDescriptor);
    }

    Set<ConstraintViolation> validateMappedConstraintSource(FacesContext facesContext,
                                                            UIComponent uiComponent,
                                                            Object convertedObject,
                                                            PropertyInformation propertyInformation)
    {
        Class baseBeanClass = this.bviUtils.getBaseClassType(propertyInformation);
        String propertyName = this.bviUtils.getPropertyToValidate(propertyInformation);
        String originalKey = getKey(propertyInformation);

        PropertyDetails constraintSourcePropertyDetails =
                resolveMappedConstraintSourceFor(originalKey, baseBeanClass, propertyName);

        if(constraintSourcePropertyDetails == null)
        {
            return Collections.emptySet();
        }

        baseBeanClass = (Class) constraintSourcePropertyDetails.getBaseObject();
        propertyName = constraintSourcePropertyDetails.getProperty();

        Class[] groups = this.bviUtils.resolveGroups(facesContext, uiComponent);

        if (groups == null)
        {
            return null;
        }

        ValidatorFactory validatorFactory = ExtValBeanValidationContext.getCurrentInstance().getValidatorFactory();
        return validatorFactory
                .usingContext()
                .messageInterpolator(ExtValBeanValidationContext.getCurrentInstance().getMessageInterpolator())
                .constraintValidatorFactory(validatorFactory.getConstraintValidatorFactory())
                .traversableResolver(validatorFactory.getTraversableResolver())
                .getValidator()
                .validateValue(baseBeanClass, propertyName, convertedObject, groups);
    }

    private String getKey(PropertyInformation propertyInformation)
    {
        return ExtValUtils.getPropertyDetails(propertyInformation).getKey();
    }

    //use the PropertyDetails to avoid a new data-structure
    //an instance of the target class isn't required in for this use-case so the class itself is stored directly
    //a clean approach would require an additional api
    //however, since it's only used in this class it's ok to use casting instead
    PropertyDetails resolveMappedConstraintSourceFor(String originalKey, Class baseBeanClass, String property)
    {
        if (isMappedConstraintSourceCached(baseBeanClass, property))
        {
            return getMappedConstraintSource(baseBeanClass, property);
        }

        //unproxy class
        if(ClassUtils.isProxiedClass(baseBeanClass))
        {
            baseBeanClass = ClassUtils.tryToLoadClassForName(ClassUtils.getClassName(baseBeanClass));
        }

        Class newBaseBeanClass = findMappedClass(baseBeanClass, property);

        //mapped source is ignored via @IgnoreConstraintSource or there is just no mapping annotation at the target
        if(newBaseBeanClass == null)
        {
            tryToCacheMappedConstraintSourceMetaData(baseBeanClass, property, null);
            return null;
        }

        String newProperty = findMappedProperty(baseBeanClass, newBaseBeanClass, property);

        PropertyDetails result = new PropertyDetails(originalKey, newBaseBeanClass, newProperty);

        tryToCacheMappedConstraintSourceMetaData(baseBeanClass, property, result);
        return result;
    }

    private boolean isMappedConstraintSourceCached(Class baseBeanClass, String property)
    {
        return getConstraintSourceStorage().containsMapping(baseBeanClass, property);
    }

    private PropertyDetails getMappedConstraintSource(Class baseBeanClass, String property)
    {
        return getConstraintSourceStorage().getMappedConstraintSource(baseBeanClass, property);
    }

    private void tryToCacheMappedConstraintSourceMetaData(
            Class originalClass, String originalProperty, PropertyDetails result)
    {
        getConstraintSourceStorage().storeMapping(originalClass, originalProperty, result);
    }

    private MappedConstraintSourceStorage getConstraintSourceStorage()
    {
        return ExtValUtils
                .getStorage(MappedConstraintSourceStorage.class, MappedConstraintSourceStorage.class.getName());
    }

    private Class findMappedClass(Class baseBeanClass, String property)
    {
        Class<? extends Annotation> constraintSourceAnnotationImplementation = (Class) ExtValContext.getContext()
                .getGlobalProperty(ConstraintSource.class.getName());

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
            return extractValueOf(foundConstraintSourceAnnotation, Class.class);
        }

        return null;
    }

    private Method tryToGetMethod(Class baseBeanClass, String property)
    {
        Method method = tryToGetReadMethod(baseBeanClass, property);

        if (method == null)
        {
            method = tryToGetReadMethodManually(baseBeanClass, property);
        }
        return method;
    }

    private Method tryToGetReadMethod(Class entity, String property)
    {
        if (useBeanInfo())
        {
            try
            {
                BeanInfo beanInfo = Introspector.getBeanInfo(entity);
                for (PropertyDescriptor propertyDescriptor : beanInfo.getPropertyDescriptors())
                {
                    if (property.equals(propertyDescriptor.getName()) && propertyDescriptor.getReadMethod() != null)
                    {
                        return propertyDescriptor.getReadMethod();
                    }
                }
            }
            catch (IntrospectionException e)
            {
                //do nothing
            }
        }
        return null;
    }

    private boolean useBeanInfo()
    {
        return Boolean.TRUE.equals(ExtValContext.getContext().getGlobalProperty(BeanInfo.class.getName()));
    }

    @ToDo(value = Priority.MEDIUM, description = "refactor - it's also used in DefaultComponentMetaDataExtractor")
    private Method tryToGetReadMethodManually(Class entity, String property)
    {
        property = property.substring(0, 1).toUpperCase() + property.substring(1);

        try
        {
            //changed to official bean spec. due to caching there is no performance issue any more
            return entity.getDeclaredMethod("is" + property);
        }
        catch (NoSuchMethodException e)
        {
            try
            {
                return entity.getDeclaredMethod("get" + property);
            }
            catch (NoSuchMethodException e1)
            {
                if (logger.isTraceEnabled())
                {
                    logger.trace("method not found - class: " + entity.getName()
                            + " - methods: " + "get" + property + " " + "is" + property);
                }

                return null;
            }
        }
    }

    @ToDo(value = Priority.MEDIUM, description = "refactor - it's also used in DefaultComponentMetaDataExtractor")
    private Field tryToGetField(Class baseBeanClass, String property)
    {
        Field field;

        try
        {
            field = getDeclaredField(baseBeanClass, property);
        }
        catch (Exception e)
        {
            try
            {
                try
                {
                    field = baseBeanClass.getDeclaredField("_" + property);
                }
                catch (Exception e1)
                {
                    if (property.length() > 1 &&
                            Character.isUpperCase(property.charAt(0)) &&
                            Character.isUpperCase(property.charAt(1)))
                    {
                        //don't use Introspector#decapitalize here
                        field = baseBeanClass
                                .getDeclaredField(property.substring(0, 1).toLowerCase() + property.substring(1));
                    }
                    else
                    {
                        field = baseBeanClass.getDeclaredField(Introspector.decapitalize(property));
                    }
                }
            }
            catch (NoSuchFieldException e1)
            {
                if (logger.isTraceEnabled())
                {
                    logger.trace("field " + property + " or _" + property + " not found", e1);
                }

                return null;
            }
        }

        return field;
    }

    @ToDo.List({
      @ToDo(value = Priority.HIGH, description = "add support for instances wrapped with cglib"),
      @ToDo(value = Priority.BLOCKING, description = "refactor - it's also used in DefaultComponentMetaDataExtractor")
    })
    private Field getDeclaredField(Class entity, String property) throws NoSuchFieldException
    {
        return entity.getDeclaredField(property);
    }

    private boolean isMappedConstraintSourceIgnored(Class baseBeanClass, String property)
    {
        Method method = tryToGetMethod(baseBeanClass, property);

        if(method != null && method.isAnnotationPresent(getIgnoreConstraintSourceAnnotationImplementation()))
        {
            return true;
        }

        Field field = tryToGetField(baseBeanClass, property);

        if(field != null && field.isAnnotationPresent(getIgnoreConstraintSourceAnnotationImplementation()))
        {
            return true;
        }

        return false;
    }

    private Annotation tryToGetConstraintSourceAnnotationFromClass(
            Class baseBeanClass, Class<? extends Annotation> annotation)
    {
        if (baseBeanClass.isAnnotationPresent(annotation))
        {
            return baseBeanClass.getAnnotation(annotation);
        }
        return null;
    }

    private Class<? extends Annotation> getIgnoreConstraintSourceAnnotationImplementation()
    {
        return (Class) ExtValContext.getContext().getGlobalProperty(IgnoreConstraintSource.class.getName());
    }

    private String findMappedProperty(Class baseBeanClass, Class newBaseBeanClass, String originalProperty)
    {
        Annotation targetPropertyAnnotation = getTargetPropertyMetaData(baseBeanClass, originalProperty);
        if (targetPropertyAnnotation != null)
        {
            return extractNewPropertyName(newBaseBeanClass, targetPropertyAnnotation);
        }

        return originalProperty;
    }

    private Annotation getTargetPropertyMetaData(Class baseBeanClass, String originalProperty)
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

    private Annotation findTargetPropertyIdAnnotation(Class baseBeanClass,
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

    private Annotation findTargetPropertyAnnotation(Class baseBeanClass,
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

    private Annotation tryToGetAnnotationFromProperty(
            Class baseBeanClass, String property, Class<? extends Annotation> annotationClass)
    {
        Method method = tryToGetMethod(baseBeanClass, property);

        if (method != null && method.isAnnotationPresent(annotationClass))
        {
            return method.getAnnotation(annotationClass);
        }
        return null;
    }

    private Annotation tryToGetAnnotationFromField(
            Class baseBeanClass, String property, Class<? extends Annotation> annotationClass)
    {
        Field field = tryToGetField(baseBeanClass, property);

        if (field != null && field.isAnnotationPresent(annotationClass))
        {
            return field.getAnnotation(annotationClass);
        }
        return null;
    }

    private Class<? extends Annotation> getTargetPropertyAnnotationImplementation()
    {
        return (Class) ExtValContext.getContext().getGlobalProperty(TargetProperty.class.getName());
    }

    private Class<? extends Annotation> getTargetPropertyIdAnnotationImplementation()
    {
        return (Class) ExtValContext.getContext().getGlobalProperty(TargetPropertyId.class.getName());
    }

    private String extractNewPropertyName(Class targetClass, Annotation annotation)
    {
        Object annotationValue = extractValueOf(annotation, Object.class);

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
    private String findNameOfAnnotatedProperty(
            Class targetClass, Class<? extends Annotation> customTargetMarkerAnnotation)
    {
        for(Method currentMethod : targetClass.getDeclaredMethods())
        {
            if(currentMethod.isAnnotationPresent(customTargetMarkerAnnotation))
            {
                return convertMethodToPropertyName(currentMethod.getName());
            }
        }

        for(Field currentField : targetClass.getDeclaredFields())
        {
            if(currentField.isAnnotationPresent(customTargetMarkerAnnotation))
            {
                return convertFieldToPropertyName(currentField.getName());
            }
        }
        return null;
    }

    private String convertMethodToPropertyName(String name)
    {
        String result = name;

        if(name.startsWith("is"))
        {
            result = name.substring(2);
        }
        else if(name.startsWith("get"))
        {
            result = name.substring(3);
        }

        return Introspector.decapitalize(result);
    }

    private String convertFieldToPropertyName(String name)
    {
        if(name.startsWith("_"))
        {
            return name.substring(1);
        }
        return name;
    }

    private <T> T extractValueOf(Annotation annotation, Class<T> targetClass)
    {
        for (Method annotationMethod : annotation.annotationType().getDeclaredMethods())
        {
            if ("value".equals(annotationMethod.getName()))
            {
                try
                {
                    return (T) annotationMethod.invoke(annotation);
                }
                catch (Throwable t)
                {
                    //do nothing
                }
            }
        }
        return null;
    }
}