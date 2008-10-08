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
package org.apache.myfaces.extensions.validator.baseval.strategy;

import org.apache.myfaces.extensions.validator.baseval.annotation.JoinValidation;
import org.apache.myfaces.extensions.validator.baseval.annotation.extractor.DefaultPropertyScanningAnnotationExtractor;
import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.AnnotationExtractor;
import org.apache.myfaces.extensions.validator.core.validation.strategy.AbstractValidatorAdapter;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;

/**
 * @author Gerhard Petracek
 */
public class JoinValidationStrategy extends AbstractValidatorAdapter
{
    public void processValidation(FacesContext facesContext,
            UIComponent uiComponent, AnnotationEntry annotationEntry,
            Object convertedObject) throws ValidatorException
    {
        AnnotationExtractor extractor = new DefaultPropertyScanningAnnotationExtractor();

        String[] targetExpressions = annotationEntry.getAnnotation(JoinValidation.class).value();

        ValidationStrategy validationStrategy;

        for (String targetExpression : targetExpressions)
        {
            targetExpression = createValidBinding(annotationEntry, targetExpression);

            for (AnnotationEntry entry : extractor.extractAnnotations(facesContext, targetExpression))
            {
                validationStrategy = ExtValUtils.getValidationStrategyForAnnotation(entry.getAnnotation());

                if (validationStrategy != null)
                {
                    validationStrategy.validate(facesContext, uiComponent, entry, convertedObject);
                }
                else
                {
                    if(logger.isTraceEnabled())
                    {
                        logger.trace("no validation strategy found for "
                            + entry.getAnnotation().annotationType().getName());
                    }
                }
            }
        }
    }

    private String createValidBinding(AnnotationEntry annotationEntry, String targetExpression)
    {
        if(ExtValUtils.getELHelper().isELTerm(targetExpression))
        {
            return targetExpression;
        }

        String baseExpression = annotationEntry.getValueBindingExpression();
        if(baseExpression.contains("."))
        {
            baseExpression = baseExpression.substring(0, baseExpression.lastIndexOf("."));
        }
        else
        {
            baseExpression = baseExpression.substring(0, baseExpression.length() - 1);
        }
        return baseExpression + "." + targetExpression + "}";
    }
}
