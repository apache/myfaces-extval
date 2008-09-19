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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.AnnotationExtractor;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.Map;

/**
 * @author Gerhard Petracek
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ValidationUtils
{
    private static final Log LOGGER = LogFactory.getLog(ValidationUtils.class);

    public static void processExtValValidation(FacesContext facesContext,
                                               UIComponent uiComponent, Object convertedObject)
    {
        if (!(uiComponent instanceof EditableValueHolder))
        {
            return;
        }

        ValidationStrategy validationStrategy;

        AnnotationExtractor annotationExtractor = FactoryUtils
            .getAnnotationExtractorFactory().create();
        for (AnnotationEntry entry : annotationExtractor
            .extractAnnotations(facesContext, uiComponent))
        {
            validationStrategy = FactoryUtils
                .getValidationStrategyFactory().create(
                entry.getAnnotation());

            if (validationStrategy != null)
            {
                validationStrategy.validate(facesContext, uiComponent,
                    entry, convertedObject);
            }
            else
            {
                LOGGER.trace("no validation strategy found for "
                    + entry.getAnnotation().annotationType().getName());
            }
        }
    }

    public static boolean isValueOfComponentRequired(FacesContext facesContext, UIComponent uiComponent)
    {
        if (!(uiComponent instanceof EditableValueHolder))
        {
            return false;
        }

        ValidationStrategy validationStrategy;

        AnnotationExtractor annotationExtractor = FactoryUtils
            .getAnnotationExtractorFactory().create();
        for (AnnotationEntry entry : annotationExtractor
            .extractAnnotations(facesContext, uiComponent))
        {
            validationStrategy = FactoryUtils
                .getValidationStrategyFactory().create(
                entry.getAnnotation());

            if (validationStrategy != null && validationStrategy instanceof MetaDataExtractor)
            {
                Map<String, Object> metaData = ((MetaDataExtractor)validationStrategy)
                                                    .extractMetaData(entry.getAnnotation());
                if(metaData.containsKey("required"))
                {
                    return (Boolean)metaData.get("required");
                }
            }
        }

        return false;
    }
}
