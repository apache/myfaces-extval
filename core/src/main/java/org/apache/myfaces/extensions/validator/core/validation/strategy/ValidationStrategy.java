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
package org.apache.myfaces.extensions.validator.core.validation.strategy;

import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

/**
 * Base interface for ValidationStrategies. A validation strategy is responsible for validating a certain constraint.
 * Furthermore, it can be an adapter for other validation frameworks like bean-validation and it's used to link e.g.
 * a constraint with a message resolver.
 * 
 * @see org.apache.myfaces.extensions.validator.core.validation.strategy.AbstractValidationStrategy
 *
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
//*ValidationStrategy instead of *Validator to avoid naming confusion 
public interface ValidationStrategy
{
    /**
     * Validates the convertedObject against the given {@link MetaDataEntry}.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The JSF component that contained the value entered by the user.
     * @param metaDataEntry The data holder which stores the meta-data and some information where the meta-data was
     * around.
     * @param convertedObject Converted object of the user entered value.
     */
    void validate(FacesContext facesContext,
                  UIComponent uiComponent,
                  MetaDataEntry metaDataEntry,
                  Object convertedObject);
}
