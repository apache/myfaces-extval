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
package org.apache.myfaces.extensions.validator.core.validation;

import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;

/**
 * Evaluator which checks if the validation process should be skipped for the given component.
 *
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public interface SkipValidationEvaluator
{
    /**
     * Checks if the validation process should be skipped for the given component.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The component for which the validation should happen.
     * @param validationStrategy The validation-strategy which is responsible for validating the given entry
     * @param entry the {@link MetaDataEntry} in question
     * @return true if the validation process should be skipped, false otherwise
     */
    boolean skipValidation(FacesContext facesContext,
                           UIComponent uiComponent,
                           ValidationStrategy validationStrategy,
                           MetaDataEntry entry);
}
