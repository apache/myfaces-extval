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
package org.apache.myfaces.extensions.validator.core.metadata.extractor;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.property.DefaultPropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.storage.MetaDataStorage;
import org.apache.myfaces.extensions.validator.core.storage.PropertyStorage;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;

/**
 * Default implementation which extracts meta-data (e.g. the annotations) of the value binding of a component.
 * It extracts the meta-data of the field and the property.
 * (Also the annotations of super classes and interfaces.)
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultComponentMetaDataExtractor implements MetaDataExtractor
{
    protected final Log logger = LogFactory.getLog(getClass());

    public DefaultComponentMetaDataExtractor()
    {
        if (logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    @ToDo(Priority.MEDIUM)
    public PropertyInformation extract(FacesContext facesContext, Object object)
    {
        PropertyInformation propertyInformation = new DefaultPropertyInformation();

        //should never occur
        if (!(object instanceof UIComponent))
        {
            if (this.logger.isWarnEnabled() && object != null)
            {
                this.logger.warn(object.getClass() + " is no valid component");
            }
            return propertyInformation;
        }

        UIComponent uiComponent = (UIComponent) object;

        if (logger.isTraceEnabled())
        {
            logger.trace("start extracting meta-data of " + uiComponent.getClass().getName());
        }

        PropertyDetails propertyDetails = ExtValUtils.getELHelper().getPropertyDetailsOfValueBinding(uiComponent);

        if (propertyDetails == null)
        {
            return propertyInformation;
        }

        /*
         * get bean class and property name
         */
        Class entityClass = ProxyUtils.getUnproxiedClass(propertyDetails.getBaseObject().getClass());

        //create
        propertyInformation.setInformation(PropertyInformationKeys.PROPERTY_DETAILS, propertyDetails);

        if (isCached(entityClass, propertyDetails.getProperty()))
        {
            for (MetaDataEntry metaDataEntry : getCachedMetaData(entityClass, propertyDetails.getProperty()))
            {
                propertyInformation.addMetaDataEntry(metaDataEntry);
            }
        }
        else
        {
            extractAnnotations(propertyInformation, propertyDetails, entityClass);
            cacheMetaData(propertyInformation);
        }

        if (logger.isTraceEnabled())
        {
            logger.trace("extract finished");
        }

        return propertyInformation;
    }

    private boolean isCached(Class entityClass, String property)
    {
        return getMetaDataStorage().containsMetaDataFor(entityClass, property);
    }

    private void cacheMetaData(PropertyInformation propertyInformation)
    {
        getMetaDataStorage().storeMetaDataOf(propertyInformation);
    }

    private MetaDataEntry[] getCachedMetaData(Class entityClass, String property)
    {
        return getMetaDataStorage().getMetaData(entityClass, property);
    }

    private MetaDataStorage getMetaDataStorage()
    {
        return ExtValUtils.getStorage(MetaDataStorage.class, MetaDataStorage.class.getName());
    }

    private boolean isCachedField(Class entity, String property)
    {
        return getPropertyStorage().containsField(entity, property);
    }

    private void tryToCachedField(Class entity, String property, Field field)
    {
        PropertyStorage propertyStorage = getPropertyStorage();
        if (!propertyStorage.containsField(entity, property))
        {
            propertyStorage.storeField(entity, property, field);
        }
    }

    private Field getCachedField(Class entity, String property)
    {
        return getPropertyStorage().getField(entity, property);
    }

    private boolean isCachedMethod(Class entity, String property)
    {
        return getPropertyStorage().containsMethod(entity, property);
    }

    private void tryToCachedMethod(Class entity, String property, Method method)
    {
        PropertyStorage propertyStorage = getPropertyStorage();
        if (!propertyStorage.containsMethod(entity, property))
        {
            propertyStorage.storeMethod(entity, property, method);
        }
    }

    private Method getCachedMethod(Class entity, String property)
    {
        return getPropertyStorage().getMethod(entity, property);
    }

    private PropertyStorage getPropertyStorage()
    {
        return ExtValUtils.getStorage(PropertyStorage.class, PropertyStorage.class.getName());
    }

    protected void extractAnnotations(
            PropertyInformation propertyInformation, PropertyDetails propertyDetails, Class entityClass)
    {
        while (!Object.class.getName().equals(entityClass.getName()))
        {
            addPropertyAccessAnnotations(entityClass, propertyDetails.getProperty(), propertyInformation);
            addFieldAccessAnnotations(entityClass, propertyDetails.getProperty(), propertyInformation);

            processInterfaces(entityClass, propertyDetails, propertyInformation);

            entityClass = entityClass.getSuperclass();
        }
    }

    private void processInterfaces(
            Class currentClass, PropertyDetails propertyDetails, PropertyInformation propertyInformation)
    {
        for (Class currentInterface : currentClass.getInterfaces())
        {
            addPropertyAccessAnnotations(currentInterface, propertyDetails.getProperty(), propertyInformation);

            processInterfaces(currentInterface, propertyDetails, propertyInformation);
        }
    }

    protected void addPropertyAccessAnnotations(Class entity, String property,
                                                PropertyInformation propertyInformation)
    {
        Method method = tryToGetReadMethod(entity, property);

        if (method == null)
        {
            method = tryToGetReadMethodManually(entity, property);
        }

        if (method != null)
        {
            tryToCachedMethod(entity, property, method);
            addAnnotationToAnnotationEntries(Arrays.asList(method.getAnnotations()), propertyInformation);
        }
    }

    private Method tryToGetReadMethod(Class entity, String property)
    {
        if (isCachedMethod(entity, property))
        {
            return getCachedMethod(entity, property);
        }

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

    protected void addFieldAccessAnnotations(Class entity, String property,
                                             PropertyInformation propertyInformation)
    {
        Field field;

        try
        {
            field = getDeclaredField(entity, property);
        }
        catch (Exception e)
        {
            try
            {
                try
                {
                    field = entity.getDeclaredField("_" + property);
                }
                catch (Exception e1)
                {
                    if (property.length() > 1 &&
                            Character.isUpperCase(property.charAt(0)) &&
                            Character.isUpperCase(property.charAt(1)))
                    {
                        //don't use Introspector#decapitalize here
                        field = entity.getDeclaredField(property.substring(0, 1).toLowerCase() + property.substring(1));
                    }
                    else
                    {
                        field = entity.getDeclaredField(Introspector.decapitalize(property));
                    }
                }
            }
            catch (NoSuchFieldException e1)
            {
                if (logger.isTraceEnabled())
                {
                    logger.trace("field " + property + " or _" + property + " not found", e1);
                }

                return;
            }
        }

        if (field != null)
        {
            tryToCachedField(entity, property, field);
            addAnnotationToAnnotationEntries(Arrays.asList(field.getAnnotations()), propertyInformation);
        }
    }

    private Field getDeclaredField(Class entity, String property) throws NoSuchFieldException
    {
        if (isCachedField(entity, property))
        {
            return getCachedField(entity, property);
        }

        return entity.getDeclaredField(property);
    }

    protected void addAnnotationToAnnotationEntries(
            List<Annotation> annotations, PropertyInformation propertyInformation)
    {
        for (Annotation annotation : annotations)
        {
            propertyInformation.addMetaDataEntry(createMetaDataEntryForAnnotation(annotation));

            if (logger.isTraceEnabled())
            {
                logger.trace(annotation.getClass().getName() + " found");
            }
        }
    }

    protected MetaDataEntry createMetaDataEntryForAnnotation(Annotation foundAnnotation)
    {
        MetaDataEntry entry = new MetaDataEntry();

        entry.setKey(foundAnnotation.annotationType().getName());
        entry.setValue(foundAnnotation);

        return entry;
    }
}
