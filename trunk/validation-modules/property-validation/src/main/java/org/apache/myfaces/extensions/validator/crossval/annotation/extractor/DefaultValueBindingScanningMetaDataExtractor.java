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

import org.apache.myfaces.extensions.validator.core.metadata.SourceInformation;
import org.apache.myfaces.extensions.validator.core.metadata.DefaultSourceInformation;
import org.apache.myfaces.extensions.validator.core.metadata.PropertySourceInformationKeys;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.DefaultComponentMetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.context.FacesContext;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;

/**
 * to support the usage of vb-xpressions (to reference the target bean)
 *
 * @author Gerhard Petracek
 */
public class DefaultValueBindingScanningMetaDataExtractor extends DefaultComponentMetaDataExtractor
{

    @Override
    public SourceInformation extract(FacesContext facesContext,
            Object object)
    {
        SourceInformation sourceInformation = new DefaultSourceInformation();

        //should never occur
        if (!(object instanceof String))
        {
            return sourceInformation;
        }

        ValueBindingExpression valueBindingExpression = new ValueBindingExpression(((String) object).trim());

        Class entity = ExtValUtils.getELHelper().getTypeOfValueBindingForExpression(facesContext,
                valueBindingExpression);

        if (entity != null)
        {
            //find and add annotations
            addPropertyAccessAnnotations(entity, sourceInformation, valueBindingExpression.getExpressionString());
            addFieldAccessAnnotations(entity, sourceInformation, valueBindingExpression.getExpressionString());
        }

        return sourceInformation;
    }

    protected void addPropertyAccessAnnotations(Class entity,
            SourceInformation sourceInformation,
            String valueBindingExpression)
    {
        for (Method method : entity.getDeclaredMethods())
        {
            sourceInformation.setProperty(
                PropertySourceInformationKeys.VALUE_BINDING_EXPRESSION, valueBindingExpression);

            addAnnotationToAnnotationEntries(Arrays.asList(method.getAnnotations()), sourceInformation);
        }
    }

    protected void addFieldAccessAnnotations(Class entity,
            SourceInformation sourceInformation,
            String valueBindingExpression)
    {
        for (Field field : entity.getDeclaredFields())
        {
            sourceInformation.setProperty(
                PropertySourceInformationKeys.VALUE_BINDING_EXPRESSION, valueBindingExpression);

            addAnnotationToAnnotationEntries(Arrays.asList(field.getAnnotations()), sourceInformation);
        }
    }
}
