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
package org.apache.myfaces.extensions.validator.core;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.core.storage.FacesInformationStorage;

import javax.faces.event.PhaseListener;
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;

/**
 * e.g. to allow in metadata extraction interceptors to know if they are invoked during validation or
 * component initialization (if needed)
 * example: client-side validation - some functionality shouldn't be processed during rendering
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class PhaseIdRecordingPhaseListener implements PhaseListener
{
    private static final long serialVersionUID = 2791240514014867457L;

    public void afterPhase(PhaseEvent phaseEvent)
    {
    }

    public void beforePhase(PhaseEvent phaseEvent)
    {
        FacesInformationStorage facesInformationStorage = ExtValUtils
                .getStorage(FacesInformationStorage.class, FacesInformationStorage.class.getName());

        facesInformationStorage.setCurrentPhaseId(phaseEvent.getPhaseId());
    }

    public PhaseId getPhaseId()
    {
        return PhaseId.ANY_PHASE;
    }
}
