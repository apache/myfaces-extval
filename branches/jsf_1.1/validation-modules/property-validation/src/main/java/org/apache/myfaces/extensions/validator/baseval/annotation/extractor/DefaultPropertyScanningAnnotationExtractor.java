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
package org.apache.myfaces.extensions.validator.baseval.annotation.extractor;

import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.DefaultComponentAnnotationExtractor;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.context.FacesContext;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Gerhard Petracek
 */
public class DefaultPropertyScanningAnnotationExtractor extends
        DefaultComponentAnnotationExtractor
{

    @Override
    @ToDo(Priority.MEDIUM)
    public List<AnnotationEntry> extractAnnotations(FacesContext facesContext,
            Object object)
    {
        //should never occur
        if (!(object instanceof String))
        {
            return new ArrayList<AnnotationEntry>();
        }

        String valueBindingExpression = ((String) object).trim();
        int beanPropertyBorder = valueBindingExpression.lastIndexOf(".");

        String property = valueBindingExpression
                .substring(beanPropertyBorder + 1, valueBindingExpression
                        .lastIndexOf("}"));

        valueBindingExpression = valueBindingExpression.substring(0,
                beanPropertyBorder)
                + "}";

        List<AnnotationEntry> annotationEntries = new ArrayList<AnnotationEntry>();

        Class entityClass = ExtValUtils.getELHelper().getTypeOfValueBindingForExpression(
                facesContext, valueBindingExpression);

        //create template entry
        AnnotationEntry templateEntry = new AnnotationEntry();
        templateEntry.setEntityClass(entityClass);
        //TODO complex components
        templateEntry.setValueBindingExpression(valueBindingExpression);
        templateEntry.setBoundTo("value");

        /*
         * find and add annotations
         */
        if (entityClass != null)
        {
            addPropertyAccessAnnotations(entityClass, property,
                    annotationEntries, templateEntry);
            addFieldAccessAnnotations(entityClass, property, annotationEntries,
                    templateEntry);
        }

        return annotationEntries;
    }
}