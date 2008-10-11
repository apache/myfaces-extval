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

import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.core.metadata.SourceInformation;
import org.apache.myfaces.extensions.validator.core.metadata.DefaultSourceInformation;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.PropertySourceInformationKeys;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
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
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    @ToDo(Priority.MEDIUM)
    public SourceInformation extract(FacesContext facesContext, Object object)
    {
        SourceInformation sourceInformation = new DefaultSourceInformation();

        //should never occur
        if (!(object instanceof UIComponent))
        {
            return sourceInformation;
        }

        UIComponent uiComponent = (UIComponent) object;

        if(logger.isTraceEnabled())
        {
            logger.trace("start extracting meta-data of " + uiComponent.getClass().getName());
        }

        ValueBindingExpression vbe =
            ExtValUtils.getELHelper().getValueBindingExpression(uiComponent);

        if (vbe == null)
        {
            return sourceInformation;
        }

        /*
         * get bean class and property name
         */
        Class entityClass = ExtValUtils.getELHelper()
            .getTypeOfValueBindingForExpression(facesContext, vbe.getBaseExpression());

        //create template entry
        //TODO test with complex components
        sourceInformation.setProperty(
            PropertySourceInformationKeys.VALUE_BINDING_EXPRESSION, vbe.getExpressionString());

        /*
         * find and add annotations
         */
        Class currentClass = entityClass;

        while (!Object.class.getName().equals(currentClass.getName()))
        {
            //TODO map syntax support
            addPropertyAccessAnnotations(currentClass, vbe.getProperty(), sourceInformation);
            addFieldAccessAnnotations(currentClass, vbe.getProperty(), sourceInformation);

            currentClass = currentClass.getSuperclass();
        }

        for (Class currentInterface : entityClass.getInterfaces())
        {
            currentClass = currentInterface;

            while (currentClass != null)
            {
                addPropertyAccessAnnotations(currentClass, vbe.getProperty(), sourceInformation);

                currentClass = currentClass.getSuperclass();
            }
        }

        if(logger.isTraceEnabled())
        {
            logger.trace("extract finished");
        }

        return sourceInformation;
    }

    protected void addPropertyAccessAnnotations(Class entity, String property,
                                                SourceInformation sourceInformation)
    {
        property = property.substring(0, 1).toUpperCase() + property.substring(1);

        Method method;

        try
        {
            method = entity.getDeclaredMethod("get" + property);
        }
        catch (NoSuchMethodException e)
        {
            try
            {
                method = entity.getDeclaredMethod("is" + property);
            }
            catch (NoSuchMethodException e1)
            {
                if(logger.isTraceEnabled())
                {
                    logger.trace("method not found - class: " + entity.getName()
                        + " - methods: " + "get" + property + " " + "is" + property);
                }

                return;
            }
        }

        addAnnotationToAnnotationEntries(Arrays.asList(method.getAnnotations()), sourceInformation);
    }

    protected void addFieldAccessAnnotations(Class entity, String property,
                                             SourceInformation sourceInformation)
    {
        Field field;

        try
        {
            field = entity.getDeclaredField(property);
        }
        catch (Exception e)
        {
            try
            {
                field = entity.getDeclaredField("_" + property);
            }
            catch (NoSuchFieldException e1)
            {
                if(logger.isTraceEnabled())
                {
                    logger.trace("field " + property + " or _" + property + " not found", e1);
                }

                return;
            }
        }

        addAnnotationToAnnotationEntries(Arrays.asList(field.getAnnotations()), sourceInformation);
    }

    protected void addAnnotationToAnnotationEntries(
        List<Annotation> annotations, SourceInformation sourceInformation)
    {
        for (Annotation annotation : annotations)
        {
            sourceInformation.addMetaDataEntry(createMetaDataEntryForAnnotation(annotation));

            if(logger.isTraceEnabled())
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
