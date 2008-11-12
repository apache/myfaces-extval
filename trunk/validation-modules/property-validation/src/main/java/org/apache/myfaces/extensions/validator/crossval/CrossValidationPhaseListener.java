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
package org.apache.myfaces.extensions.validator.crossval;

import org.apache.myfaces.extensions.validator.util.CrossValidationUtils;
import org.apache.myfaces.extensions.validator.util.JsfUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.application.FacesMessage;
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;
import javax.faces.validator.ValidatorException;

/**
 * This phase listener processes cross validation as soon as it finds a special request scoped storage.<br/>
 * So it's possible to add information during the process validation phase. At the end of this phase it gets processed.
 * After that the storage gets reseted.<p/>
 * If you provide a custom extension and you add the same storage type within a different phase,
 * it also gets processed at the end of that phase.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class CrossValidationPhaseListener implements PhaseListener
{
    private boolean isInitialized = false;

    public void afterPhase(PhaseEvent event)
    {
        try
        {
            CrossValidationStorage crossValidationStorage = CrossValidationUtils.getOrInitCrossValidationStorage();
            for (CrossValidationStorageEntry entry : crossValidationStorage.getCrossValidationStorageEntries())
            {
                try
                {
                    entry.getValidationStrategy().processCrossValidation(entry, crossValidationStorage);
                }
                catch (ValidatorException e)
                {
                    if(ExtValUtils.executeAfterThrowingInterceptors(
                            entry.getComponent(), entry.getMetaDataEntry(), entry.getConvertedObject(), e))
                    {
                        FacesMessage facesMessage = e.getFacesMessage();

                        if (facesMessage != null &&
                                facesMessage.getSummary() != null && facesMessage.getDetail() != null)
                        {
                            event.getFacesContext().addMessage(entry.getClientId(), facesMessage);
                        }

                        event.getFacesContext().renderResponse();
                    }
                }
            }
        }
        finally
        {
            CrossValidationUtils.resetCrossValidationStorage();
        }
    }

    public void beforePhase(PhaseEvent event)
    {
        if (!isInitialized)
        {
            if (WebXmlParameter.DEACTIVATE_CROSSVALIDATION != null
                    && WebXmlParameter.DEACTIVATE_CROSSVALIDATION.equalsIgnoreCase("true"))
            {
                JsfUtils.deregisterPhaseListener(this);
            }
            else
            {
                isInitialized = true;
            }
        }
    }

    public PhaseId getPhaseId()
    {
        return PhaseId.ANY_PHASE;
    }
}
