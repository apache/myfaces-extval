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

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.util.ELUtils;

/**
 * @author Gerhard Petracek
 */
public class DefaultComponentAnnotationExtractor implements AnnotationExtractor
{
    protected final Log logger = LogFactory.getLog(getClass());

    public List<AnnotationEntry> extractAnnotations(FacesContext facesContext,
            Object object)
    {
        //should never occur
        if (!(object instanceof UIComponent))
        {
            //TODO
            return new ArrayList<AnnotationEntry>();
        }

        UIComponent uiComponent = (UIComponent) object;

        List<AnnotationEntry> annotationEntries = new ArrayList<AnnotationEntry>();

        String valueBindingExpression = ELUtils
                .getReliableValueBindingExpression(uiComponent);

        if (valueBindingExpression == null)
        {
            return new ArrayList<AnnotationEntry>();
        }

        /*
         * get bean class and property name
         */
        int beanPropertyBorder = valueBindingExpression.lastIndexOf('.');

        if (beanPropertyBorder < 0)
        {
            return new ArrayList<AnnotationEntry>();
        }

        String beans = valueBindingExpression.substring(valueBindingExpression
                .indexOf('{') + 1, beanPropertyBorder);

        String property = valueBindingExpression.substring(
                beanPropertyBorder + 1, valueBindingExpression.indexOf('}'));

        Class entityClass = ELUtils.getTypeOfValueBindingForExpression(
                facesContext, "#{" + beans + "}");

        //create template entry
        AnnotationEntry templateEntry = new AnnotationEntry();
        //TODO test with complex components
        templateEntry.setValueBindingExpression(valueBindingExpression);
        templateEntry.setEntityClass(entityClass);
        //TODO
        templateEntry.setBoundTo("value");

        /*
         * find and add annotations
         */
        Class currentClass = entityClass;

        while (!Object.class.getName().equals(currentClass.getName()))
        {
            addPropertyAccessAnnotations(currentClass, property,
                    annotationEntries, templateEntry);
            addFieldAccessAnnotations(currentClass, property,
                    annotationEntries, templateEntry);

            currentClass = currentClass.getSuperclass();
        }

        for (Class currentInterface : entityClass.getInterfaces())
        {
            currentClass = currentInterface;

            while (currentClass != null)
            {
                addPropertyAccessAnnotations(currentClass, property,
                        annotationEntries, templateEntry);

                currentClass = currentClass.getSuperclass();
            }
        }

        return annotationEntries;
    }

    protected void addPropertyAccessAnnotations(Class entity, String property,
            List<AnnotationEntry> annotationEntries,
            AnnotationEntry templateEntry)
    {
        property = property.substring(0, 1).toUpperCase()
                + property.substring(1);

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
                logger.debug("method not found - class: " + entity.getName()
                        + " - methods: " + "get" + property + " " + "is"
                        + property);
                return;
            }
        }

        addAnnotationToAnnotationEntries(annotationEntries, Arrays
                .asList(method.getAnnotations()), templateEntry);
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
                logger.debug("field " + property + " or _" + property
                        + " not found");
                return;
            }
        }

        addAnnotationToAnnotationEntries(annotationEntries, Arrays.asList(field
                .getAnnotations()), templateEntry);
    }

    protected void addAnnotationToAnnotationEntries(
            List<AnnotationEntry> annotationEntries,
            List<Annotation> annotations, AnnotationEntry templateEntry)
    {
        for (Annotation annotation : annotations)
        {
            annotationEntries.add(createAnnotationEntry(annotation,
                    templateEntry));
        }
    }

    protected AnnotationEntry createAnnotationEntry(Annotation foundAnnotation,
            AnnotationEntry templateEntry)
    {
        AnnotationEntry entry = new AnnotationEntry();

        entry.setAnnotation(foundAnnotation);

        entry.setEntityClass(templateEntry.getEntityClass());
        entry.setValueBindingExpression(templateEntry
                .getValueBindingExpression());
        entry.setBoundTo(templateEntry.getBoundTo());

        return entry;
    }
}
