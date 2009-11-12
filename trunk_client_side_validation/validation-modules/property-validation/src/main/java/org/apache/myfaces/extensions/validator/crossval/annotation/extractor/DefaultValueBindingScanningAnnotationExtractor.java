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
package org.apache.myfaces.extensions.validator.crossval.annotation.extractor;

import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.DefaultComponentAnnotationExtractor;
import org.apache.myfaces.extensions.validator.util.ELUtils;

import javax.faces.context.FacesContext;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * to support the usage of vb-xpressions (to reference the target bean)
 *
 * @author Gerhard Petracek
 */
public class DefaultValueBindingScanningAnnotationExtractor extends
        DefaultComponentAnnotationExtractor
{

    @Override
    public List<AnnotationEntry> extractAnnotations(FacesContext facesContext,
            Object object)
    {
        //should never occur
        if (!(object instanceof String))
        {
            return new ArrayList<AnnotationEntry>();
        }

        String valueBindingExpression = ((String) object).trim();

        List<AnnotationEntry> annotationEntries = new ArrayList<AnnotationEntry>();

        Class entity = ELUtils.getTypeOfValueBindingForExpression(facesContext,
                valueBindingExpression);

        if (entity != null)
        {
            //find and add annotations
            addPropertyAccessAnnotations(entity, annotationEntries,
                    valueBindingExpression);
            addFieldAccessAnnotations(entity, annotationEntries,
                    valueBindingExpression);
        }

        return annotationEntries;
    }

    protected void addPropertyAccessAnnotations(Class entity,
            List<AnnotationEntry> annotationEntries,
            String valueBindingExpression)
    {
        AnnotationEntry templateEntry;

        for (Method method : entity.getDeclaredMethods())
        {
            templateEntry = new AnnotationEntry();
            templateEntry.setEntityClass(entity.getClass());
            templateEntry.setValueBindingExpression(valueBindingExpression);
            templateEntry.setBoundTo("[method]:" + method.getName());

            addAnnotationToAnnotationEntries(annotationEntries, Arrays
                    .asList(method.getAnnotations()), templateEntry);
        }
    }

    protected void addFieldAccessAnnotations(Class entity,
            List<AnnotationEntry> annotationEntries,
            String valueBindingExpression)
    {
        AnnotationEntry templateEntry;

        for (Field field : entity.getDeclaredFields())
        {
            templateEntry = new AnnotationEntry();
            templateEntry.setEntityClass(entity.getClass());
            templateEntry.setValueBindingExpression(valueBindingExpression);
            templateEntry.setBoundTo("[field]:" + field.getName());

            addAnnotationToAnnotationEntries(annotationEntries, Arrays
                    .asList(field.getAnnotations()), templateEntry);
        }
    }
}
