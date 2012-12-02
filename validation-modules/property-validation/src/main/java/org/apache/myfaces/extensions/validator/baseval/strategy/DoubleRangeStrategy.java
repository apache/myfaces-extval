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

import org.apache.myfaces.extensions.validator.baseval.annotation.DoubleRange;
import org.apache.myfaces.extensions.validator.baseval.annotation.SkipValidationSupport;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.validation.strategy.AbstractValidationStrategy;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.DoubleRangeValidator;
import javax.faces.validator.ValidatorException;

/**
 * @since 1.x.1
 */
@SkipValidationSupport
@UsageInformation(UsageCategory.INTERNAL)
public class DoubleRangeStrategy extends AbstractValidationStrategy
{
    protected void processValidation(FacesContext facesContext,
            UIComponent uiComponent, MetaDataEntry metaDataEntry,
            Object convertedObject) throws ValidatorException
    {

        DoubleRange annotation = metaDataEntry.getValue(DoubleRange.class);
        DoubleRangeValidator doubleRangeValidator = (DoubleRangeValidator)facesContext.getApplication()
                                                        .createValidator("javax.faces.DoubleRange");

        if(annotation.minimum() != Double.MIN_VALUE)
        {
            doubleRangeValidator.setMinimum(annotation.minimum());
        }

        if(annotation.maximum() != Double.MAX_VALUE)
        {
            doubleRangeValidator.setMaximum(annotation.maximum());
        }

        doubleRangeValidator.validate(facesContext, uiComponent, convertedObject);
    }
}
