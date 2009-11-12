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
package org.apache.myfaces.extensions.validator.test.baseval.parameter;

import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.strategy.AbstractAnnotationValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ParameterValue;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.validator.ValidatorException;
import java.lang.annotation.Annotation;

public class AdditionalValidator extends TestValidatorProvider
{
    @ParameterValue
    public Class getProviderClass()
    {
        return AdditionalValidator.class;
    }

    @Override
    public ValidationStrategy getValidationStrategy()
    {
        return new AbstractAnnotationValidationStrategy()
        {
            protected String getValidationErrorMsgKey(Annotation annotation)
            {
                return "validation_failed";
            }

            protected void processValidation(FacesContext facesContext, UIComponent uiComponent, MetaDataEntry metaDataEntry, Object convertedObject) throws ValidatorException
            {
            }
        };
    }
}