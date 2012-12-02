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

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;

/**
 * To map constraints directly to a meta-data transformer if there is no validation strategy (required by jsr 303).
 * So the ValidationStrategy is only used to have the component initialization and is not used to do the actual
 * validation.
 *
 * @since x.x.3
 */
@UsageInformation(UsageCategory.REUSE)
public abstract class AbstractVirtualValidationStrategy implements IdentifiableValidationStrategy
{
    /**
     * {@inheritDoc}
     * Throws an unsupportedOperationException when the method gets executed. This ValidationStrategy should never
     * be used to perform actual validations.  
     */
    public final void validate(
            FacesContext facesContext, UIComponent uiComponent, MetaDataEntry metaDataEntry, Object convertedObject)
    {
        throw new UnsupportedOperationException("this is just an adapter e.g. for component initialization");
    }
}
