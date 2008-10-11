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
import org.apache.myfaces.extensions.validator.baseval.annotation.extractor.DefaultPropertyScanningMetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.PropertySourceInformationKeys;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.validation.strategy.AbstractValidatorAdapter;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
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
            UIComponent uiComponent, MetaDataEntry metaDataEntry,
            Object convertedObject) throws ValidatorException
    {
        MetaDataExtractor extractor = new DefaultPropertyScanningMetaDataExtractor();

        String[] targetExpressions = metaDataEntry.getValue(JoinValidation.class).value();

        ValidationStrategy validationStrategy;

        for (String targetExpression : targetExpressions)
        {
            targetExpression = createValidBinding(metaDataEntry, targetExpression);

            for (MetaDataEntry entry : extractor.extract(facesContext, targetExpression).getMetaDataEntries())
            {
                validationStrategy = ExtValUtils.getValidationStrategyForMetaData(entry.getKey());

                if (validationStrategy != null)
                {
                    validationStrategy.validate(facesContext, uiComponent, entry, convertedObject);
                }
                else
                {
                    if(logger.isTraceEnabled())
                    {
                        logger.trace("no validation strategy found for " + entry.getValue());
                    }
                }
            }
        }
    }

    private String createValidBinding(MetaDataEntry metaDataEntry, String targetExpression)
    {
        if(ExtValUtils.getELHelper().isELTerm(targetExpression))
        {
            return targetExpression;
        }

        ValueBindingExpression baseExpression = new ValueBindingExpression(
            metaDataEntry.getProperty(PropertySourceInformationKeys.VALUE_BINDING_EXPRESSION, String.class));
        return ValueBindingExpression.replaceOrAddProperty(baseExpression, targetExpression).getExpressionString();
    }
}
