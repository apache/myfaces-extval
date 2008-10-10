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
package org.apache.myfaces.extensions.validator.core.annotation.extractor;

import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Default implementation which extracts the annotations of the value binding of a component.
 * It extracts the annotation of the field and the property.
 * (Also the annotations of super classes and interfaces.)
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultComponentAnnotationExtractor implements AnnotationExtractor
{
    protected final Log logger = LogFactory.getLog(getClass());

    public DefaultComponentAnnotationExtractor()
    {
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    @ToDo(Priority.MEDIUM)
    public List<AnnotationEntry> extractAnnotations(FacesContext facesContext, Object object)
    {
        //should never occur
        if (!(object instanceof UIComponent))
        {
            return new ArrayList<AnnotationEntry>();
        }

        UIComponent uiComponent = (UIComponent) object;

        if(logger.isTraceEnabled())
        {
            logger.trace("start extracting annotations of " + uiComponent.getClass().getName());
        }

        List<AnnotationEntry> annotationEntries = new ArrayList<AnnotationEntry>();

        ValueBindingExpression vbe =
            ExtValUtils.getELHelper().getValueBindingExpression(uiComponent);

        if (vbe == null)
        {
            return new ArrayList<AnnotationEntry>();
        }

        /*
         * get bean class and property name
         */
        Class entityClass = ExtValUtils.getELHelper()
            .getTypeOfValueBindingForExpression(facesContext, vbe.getBaseExpression());

        //create template entry
        AnnotationEntry templateEntry = new AnnotationEntry();
        //TODO test with complex components
        templateEntry.setValueBindingExpression(vbe.getExpressionString());
        templateEntry.setEntityClass(entityClass);
        templateEntry.setBoundTo("value");

        /*
         * find and add annotations
         */
        Class currentClass = entityClass;

        while (!Object.class.getName().equals(currentClass.getName()))
        {
            //TODO map syntax support
            addPropertyAccessAnnotations(currentClass, vbe.getProperty(), annotationEntries, templateEntry);
            addFieldAccessAnnotations(currentClass, vbe.getProperty(), annotationEntries, templateEntry);

            currentClass = currentClass.getSuperclass();
        }

        for (Class currentInterface : entityClass.getInterfaces())
        {
            currentClass = currentInterface;

            while (currentClass != null)
            {
                addPropertyAccessAnnotations(currentClass, vbe.getProperty(), annotationEntries, templateEntry);

                currentClass = currentClass.getSuperclass();
            }
        }

        if(logger.isTraceEnabled())
        {
            logger.trace("extractAnnotations finished");
        }

        return annotationEntries;
    }

    protected void addPropertyAccessAnnotations(Class entity, String property,
                                                List<AnnotationEntry> annotationEntries,
                                                AnnotationEntry templateEntry)
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

        addAnnotationToAnnotationEntries(annotationEntries, Arrays.asList(method.getAnnotations()), templateEntry);
    }

    protected void addFieldAccessAnnotations(Class entity, String property,
                                             List<AnnotationEntry> annotationEntries,
                                             AnnotationEntry templateEntry)
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

        addAnnotationToAnnotationEntries(annotationEntries, Arrays.asList(field.getAnnotations()), templateEntry);
    }

    protected void addAnnotationToAnnotationEntries(
        List<AnnotationEntry> annotationEntries,
        List<Annotation> annotations, AnnotationEntry templateEntry)
    {
        for (Annotation annotation : annotations)
        {
            annotationEntries.add(createAnnotationEntry(annotation, templateEntry));

            if(logger.isTraceEnabled())
            {
                logger.trace(annotation.getClass().getName() + " found");
            }
        }
    }

    protected AnnotationEntry createAnnotationEntry(Annotation foundAnnotation, AnnotationEntry templateEntry)
    {
        AnnotationEntry entry = new AnnotationEntry();

        entry.setAnnotation(foundAnnotation);

        entry.setEntityClass(templateEntry.getEntityClass());
        entry.setValueBindingExpression(templateEntry.getValueBindingExpression());
        entry.setBoundTo(templateEntry.getBoundTo());

        return entry;
    }
}
