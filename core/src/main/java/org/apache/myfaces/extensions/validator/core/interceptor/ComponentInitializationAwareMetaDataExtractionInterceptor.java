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
package org.apache.myfaces.extensions.validator.core.interceptor;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.InvocationOrderSupport;
import org.apache.myfaces.extensions.validator.core.storage.FacesInformationStorage;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.event.PhaseId;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@InvocationOrderSupport
@UsageInformation(UsageCategory.API)
public abstract class ComponentInitializationAwareMetaDataExtractionInterceptor implements MetaDataExtractionInterceptor
{
    public final void afterExtracting(PropertyInformation propertyInformation)
    {
        if(isRenderResponsePhase())
        {
            afterExtractingForComponentInitialization(propertyInformation);
        }
    }

    protected abstract void afterExtractingForComponentInitialization(PropertyInformation propertyInformation);

    private boolean isRenderResponsePhase()
    {
        return PhaseId.RENDER_RESPONSE.equals(getFacesInformationStorage().getCurrentPhaseId());
    }

    private FacesInformationStorage getFacesInformationStorage()
    {
        return ExtValUtils.getStorage(FacesInformationStorage.class, FacesInformationStorage.class.getName());
    }
}
